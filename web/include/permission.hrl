-ifndef(__PERMISSION_ACV__).
-define(__PERMISSION_ACV__, true).

-record(permission, {
    id=null,
    name,
    alias,
    type
}).

-endif. %%% __PERMISSION_ACV__
