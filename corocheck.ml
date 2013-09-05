(*
 * Copyright (c) 2013
 *  Gabriel Kerneis     <gabriel@kerneis.info>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

open Cil
open Feature
open Pretty
module E = Errormsg

(* Adapted from cpc.ml *)
let visitReturnType visitor t = match unrollType t with
  | TFun(rt, _args, _isva, a) ->
      ignore(visitCilType visitor rt);
      ignore(visitCilAttributes visitor a)
  | _ -> E.s (E.bug "expected a function type")

let check_type_attr attr ft =
  let coop_attr = ref false in
  visitReturnType
    (object(self)
      inherit nopCilVisitor
      method vtype t = match t with
      | TFun _ -> SkipChildren
      | _ -> DoChildren
      method vattr (Attr (a, _)) = if a = attr then coop_attr := true; SkipChildren
    end)
    ft;
  !coop_attr

let coop_attr_name = ref "coroutine_fn"
let blocking_attr_name = ref "blocking_fn"

let is_coop_type = check_type_attr !coop_attr_name
let is_blocking_type = check_type_attr !blocking_attr_name

(* Common operations on varinfo *)
module V = struct
  type t = varinfo
  let compare x y = compare x.vid y.vid
  let hash x = x.vid
  let equal x y = (=) x.vid y.vid
end

(* Work-around a limitation of ocamlgraph <= 1.8.2 where Fixpoint works
 * on labels of labeled graphs instead of edges: use a labeled graph and
 * store src and dst in the label *)
module Edge = struct
  type t = V.t * V.t
  let compare = compare
  let default =
    let dummy = makeVarinfo false "dummy" voidType in
    (dummy, dummy)
end

(* Directed labeled graph of varinfo *)
module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled(V)(Edge) ;;

(* work-around: store a copy of caller and callee in label *)
let add_labeled_edge g src dst =
  G.add_edge_e g (G.E.create src (src, dst) dst)

let g = G.create()

(* Sets of declared and defined functions *)
module VS = Set.Make(V)
let decl = ref VS.empty
let def = ref VS.empty

(* Specify which functions should be trusted as cooperative. Those are the
 * functions that are declared but not defined, as well as function the address
 * of which is taken (to be conservative) *)
let is_trusted_coop v =
  is_coop_type v.vtype && (VS.mem v (VS.diff !decl !def) || v.vaddrof)

(* Backward reachability analysis for varinfo.
 * If you initialize with a set of trusted (external for instance)
 * cooperative functions, and build a call graph, the backward reachable varinfo
 * are the functions that should be cooperative. *)
module Reachability = Graph.Fixpoint.Make(G)
(struct
    type vertex = G.E.vertex
    type label = G.E.label
    type cfg = G.t
    type data = bool
    let direction = Graph.Fixpoint.Backward
    let equal = (=)
    let join = (||)
    let analyze (src, dst) coop_callee =
      is_trusted_coop src || coop_callee
end)

(* vertex collecter *)
let vregister var set =
  G.add_vertex g var;
  set := VS.add var !set

(* Whether to display native function declarations.
 * Ignoring them makes the graph more readable, and
 * does not change the result. *)
let full_graph = ref false

(* XXX could fail with composed suffixes such as .cil.i,
 * but we shouldn't have any in practice *)
let same_file f f' =
  let chop x = try Filename.chop_extension x with _ -> x in
  let fc = chop (Filename.basename f) in
  let fc' = chop (Filename.basename f') in
  assert(chop fc = fc);
  assert(chop fc' = fc');
  fc = fc'

class vcollect = fun filename ->
  object(self)
  inherit nopCilVisitor

  method vglob = function
  | GVarDecl(v,_) when isFunctionType v.vtype ->
      if  (!full_graph || is_coop_type v.vtype || is_blocking_type v.vtype) then
      vregister v decl;
      SkipChildren
  | GFun ({svar=v},{file = f}) ->
      (* XXX if an included definition misses coop declaration, we will ignore it
       * --- but CPC will refuse to compile anyway; use --fullInferenceGraph
       * if you want to be sure *)
      if (!full_graph || is_coop_type v.vtype || is_blocking_type v.vtype || same_file f filename) then
        vregister v def;
      DoChildren
  | _ -> SkipChildren
end

(* edge collecter *)

let add_call caller callee =
  (* avoid implicit creation of vertices *)
  let all_fun = VS.union !decl !def in
  if(VS.mem caller all_fun && VS.mem callee all_fun)
  then add_labeled_edge g caller callee

class ecollect =
  object(self)
  inherit nopCilVisitor
  val mutable ef = dummyFunDec

  method vfunc f =
    let f' = ef in ef <- f;
    ChangeDoChildrenPost(f, fun f -> ef <- f';f)

  method vinst = function
  | Call(_, Lval(Var v, NoOffset), _, _) ->
      add_call ef.svar v;
      SkipChildren
  | Call(_, e, _, _) ->
      let v = makeVarinfo false (sprint 20 (dn_exp () e)) (typeOf e) in
      vregister v decl;
      add_call ef.svar v;
      SkipChildren
  | _ -> SkipChildren
end

(* Function pointer assignement checker *)

let returnTypeOf e = match unrollType (typeOf e) with
| TFun (rt, _, _, _) -> rt
| _ -> E.s (E.bug "expected a function type")

let rec broken_cast t t' = match unrollType t, unrollType t' with
| TPtr (t, _), t' | t, TPtr (t', _) -> broken_cast t t'
| (TFun _ as t), (TFun _ as t') -> is_coop_type t <> is_coop_type t'
| _ -> false

class coopptrcheck =
  object(self)
  inherit nopCilVisitor

  (* For initializers and Set, casts are inserted by CIL, no need to check in
   * vinit *)
  method vexpr = function
  | (CastE (t, e) as exp) when broken_cast (typeOf e) t ->
      E.warn "wrong %s function pointer cast (at %a):\n%a" !coop_attr_name d_loc !currentLoc dn_exp exp ;
      DoChildren
  | _ -> DoChildren

  (* For Call, the cast is left implicit and only pretty-printed,
   * so we need to check separately *)
  method vinst = function
  | Call (Some lv, e, _, loc) when broken_cast (typeOfLval lv) (returnTypeOf e) ->
        E.warn "wrong %s function pointer assignment (at %a)" !coop_attr_name d_loc !currentLoc;
        DoChildren
  | _ -> DoChildren
end

(* Graphviz rendering *)

let should_be_coop = ref (fun _ -> failwith "run analysis first")

module ColoredG = struct
  include G
  let graph_attributes g = []
  let default_vertex_attributes g = []
  let vertex_name v =
    Printf.sprintf "\"%s%s%s\""
    (if is_coop_type v.vtype then (!coop_attr_name^" ") else "")
    (if is_blocking_type v.vtype then (!blocking_attr_name^" ") else "")
    v.vname
  let vertex_attributes v =
    let shape =
      if !should_be_coop v then `Box else `Ellipse in
    let color =
      if !should_be_coop v != is_coop_type v.vtype
      then 0xff0000 else 0x000000 in
    let style =
      if !should_be_coop v != is_coop_type v.vtype
      then `Dashed else `Solid in
    [`Shape shape; `Color color; `Style style]
  let get_subgraph v = None
  let default_edge_attributes g = []
  let edge_attributes e =
    let src, dst = G.E.src e, G.E.dst e in
    if is_blocking_type src.vtype &&
    (!should_be_coop dst || is_coop_type dst.vtype)
    then [ `Style `Dashed; `Color 0xff0000 ]
    else []
end

module Draw = Graph.Graphviz.Dot(ColoredG) ;;

let draw filename g =
  let chan = open_out filename in
  Draw.output_graph chan g;
  close_out chan

let check_blocking v =
  let callers = G.succ g v in
  List.iter (fun c -> match !should_be_coop c, is_coop_type c.vtype with
    | true, true -> E.warn "forbidden call:  %s %s called by %s %s"
        !blocking_attr_name v.vname !coop_attr_name c.vname
    | true, false -> E.warn "suspicious call: %s %s called by (missing) %s %s"
        !blocking_attr_name  v.vname !coop_attr_name c.vname
    | false, true -> E.warn "suspicious call: %s %s called by (spurious) %s %s"
        !blocking_attr_name v.vname !coop_attr_name c.vname
    | false, false -> ()
  ) callers

let print_warnings () =
  G.iter_vertex (function v ->
    match !should_be_coop v, is_coop_type v.vtype, is_blocking_type v.vtype with
    | _, true, true -> E.warn "conflicting %s/%s annotation: %s" !coop_attr_name !blocking_attr_name v.vname
    | true, false, false -> E.warn "missing %s annotation: %s" !coop_attr_name v.vname
    | false, true, false -> E.warn "spurious %s annotation: %s" !coop_attr_name v.vname
    | true, false, true -> E.warn "wrong %s annotation: %s" !blocking_attr_name v.vname; check_blocking v
    | _, _, true -> check_blocking v
    | _, _, _ -> ()
  ) g

let rmtmps f = Rmtmps.removeUnusedTemps f; f

let dot_file = ref ""

let doit file =
  let file =
    if !Rmtmps.keepUnused
      (* work on a copy of the file, with unused variables removed *)
      then rmtmps { (file) with fileName = file.fileName }
      else file in
  let dot_file_name =
    if !dot_file <> "" then !dot_file
    else (Filename.chop_extension file.fileName)^".dot" in
  if not !insertImplicitCasts then
    E.warn "use --insertImplicitCasts if you want to detect wrong function pointer assignments";
  visitCilFileSameGlobals (new coopptrcheck) file;
  visitCilFileSameGlobals (new vcollect file.fileName) file;
  visitCilFileSameGlobals (new ecollect) file;
  should_be_coop := Reachability.analyze is_trusted_coop g;
  draw dot_file_name g;
  print_warnings ()

let feature : Feature.t =
  { fd_name = "CoroCheck";
    fd_enabled = false;
    fd_description = "checking and inference of cooperation annotations";
    fd_extraopt = [
      ("--coopAttr", Arg.Set_string coop_attr_name,
        Printf.sprintf "<name> cooperative attribute (default: %s)" !coop_attr_name);
      ("--blockAttr", Arg.Set_string blocking_attr_name,
        Printf.sprintf "<name> blocking attribute (default: %s)" !blocking_attr_name);
      ("--dotFile", Arg.Set_string dot_file, "<name> output .dot filename");
      ("--fullInferenceGraph",Arg.Set full_graph," show all native functions in the call graph");
    ];
    fd_doit = doit;
    fd_post_check = true;
  }

let () = Feature.register feature
