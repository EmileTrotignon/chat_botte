open Async

include Disml_aux_kernel

let members_of_role_id role_id guild_id =
  let%map members = MCache.get_members guild_id in
  Member.Set.filter ~f:(fun m -> Member.has_role m role_id) members