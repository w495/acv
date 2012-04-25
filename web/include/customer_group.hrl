-ifndef(__CUSTOMER_GROUP_ACV__).
-define(__CUSTOMER_GROUP_ACV__, true).


-record(customer_group, {
    id=null,
    name,
    description,
    updater
}).


-endif. %%% __CUSTOMER_GROUP_ACV__
