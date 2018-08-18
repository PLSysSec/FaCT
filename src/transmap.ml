open Util
open Pos
open Err
open Tast
open Tast_util

let sbool = fake_pos @> Bool (fake_pos @> Secret)
let sband a b = (fake_pos @> BinOp(Ast.LogicalAnd,a,b), sbool)

(* This is a placeholder that will be replaced by the proper
   expr during the branch removal pass *)
let __ctx = (fake_pos @> Variable (fake_pos @> "__ctx"), sbool)

class transmap m =
  object (visit)
    inherit Tastmap.tast_visitor m as super
    val _secretflow : expr stack = Stack.create ()

    method block (blk_,next_) =
      let p = blk_.pos in
        match blk_.data with
          | If (cond,thens,elses) ->
            let cond' = visit#expr cond in
              if (label_of (type_of cond')).data = Secret then
                let ncond = (p@>UnOp(Ast.LogicalNot,cond'), type_of cond') in
                  push cond' _secretflow;
                  let thens' = visit#block thens in
                    pop _secretflow |> ignore;
                    push ncond _secretflow;
                    let elses' = visit#block elses in
                      pop _secretflow |> ignore;
                      (p@>If (cond',thens',elses'),visit#next next_)
              else super#block (blk_,next_)
          | _ -> super#block (blk_,next_)

  end
