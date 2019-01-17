-module(admin0).

%% @doc Admin panel access.
%% Limits access to whatever might break a system and requires a password.

-export([
         authenticate/1,
         break_system/1
        ]).

-export([test/0]).

-record(admin, {name :: binary(),
                pass :: binary(),
                acl = [] :: list()}).

authenticate(#admin{pass = Pass} = Admin) ->
    ValidPassword = <<"alamakota">>,
    case Pass of
        ValidPassword -> Admin;
        _             -> {error, invalid_password}
    end.

break_system(#admin{acl = ACL}) ->
    case lists:member(?FUNCTION_NAME, ACL) of
        true  -> {ok, broken};
        false -> {error, no_access}
    end.

test() ->
    BeforeAuth = #admin{name = <<"joe">>, pass = <<"alamakota">>},
    Admin = authenticate(BeforeAuth),
    AdminWithPower = Admin#admin{acl = [break_system]},
    [ break_system(BeforeAuth),
      break_system(Admin),
      break_system(AdminWithPower) ].
