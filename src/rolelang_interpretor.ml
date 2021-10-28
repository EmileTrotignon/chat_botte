open Core
open Async
open Rolelang
open Disml_aux
open Disml
module TmpMember = Member
open Models
module Member = TmpMember

let eval_id guild_id id =
  match id with
  | Ast.Everyone ->
      MCache.get_members guild_id
  | Ast.User id -> (
      let user_id = `User_id id in
      let member = member_of_id user_id guild_id in
      match%map member with
      | Ok member ->
          Member.Set.singleton member
      | Error _e ->
          Member.Set.empty )
  | Ast.Role id -> (
      let%bind role = role_of_id guild_id (`Role_id id) in
      Role.(
        match role with
        | None ->
            Deferred.return Member.Set.empty
        | Some role ->
            members_of_role_id role.id guild_id) )

let rec eval guild_id id =
  let eval = eval guild_id
  and eval_id = eval_id guild_id in
  Ast.(
    match id with
    | Id id ->
        eval_id id
    | Or (e1, e2) ->
        let%bind s1 = eval e1 in
        let%map s2 = eval e2 in
        Member.Set.union s1 s2
    | And (e1, e2) ->
        let%bind s1 = eval e1 in
        let%map s2 = eval e2 in
        Member.Set.inter s1 s2)
