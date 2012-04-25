-ifndef(__CUSTOMER_ACV__).
-define(__CUSTOMER_ACV__, true).

-record(customer, {
    id=null,
    firstname,
    lastname,
    patronimic,
    phone,
    type_id,
    updater,
    login,
    password_hash,
    commentary,
    type_name
}).

-endif. %% __CUSTOMER_ACV__
