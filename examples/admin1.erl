-module(admin1).

%% @doc Admin panel access.
%% Limits access to whatever might break a system and requires a password.

-export([
         authenticate/1,
         break_system/1
        ]).

-export([test/0]).

%% Make illegal states unrepresentable - https://youtu.be/kZ1P8cHN3pY?t=1287
-record(admin_before_auth, {name :: binary(),
                            pass :: binary()}).
-record(admin, {name :: binary(),
                acl = [] :: list()}).

authenticate(#admin_before_auth{name = Name, pass = Pass}) ->
    ValidPassword = <<"alamakota">>,
    case Pass of
        ValidPassword -> #admin{name = Name};
        _             -> {error, invalid_password}
    end.

break_system(#admin{acl = ACL}) ->
    case lists:member(?FUNCTION_NAME, ACL) of
        true  -> {ok, broken};
        false -> {error, no_access}
    end.

test() ->
    BeforeAuth = #admin_before_auth{name = <<"joe">>, pass = <<"alamakota">>},
    Admin = authenticate(BeforeAuth),
    AdminWithPower = Admin#admin{acl = [break_system]},
    [ catch break_system(BeforeAuth),
      break_system(Admin),
      break_system(AdminWithPower) ].
