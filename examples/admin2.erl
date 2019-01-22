-module(admin2).

%% @doc Admin panel access.
%% Limits access to whatever might break a system and requires a password.

-export([
         authenticate/1,
         break_system/1
        ]).

-export([test/0]).

-export_type([t/0,
              t_before_auth/0]).

%% Make illegal states unrepresentable - https://youtu.be/kZ1P8cHN3pY?t=1287
-type t()             :: #{name := binary(),
                           acl  := [atom()]}.
-type t_before_auth() :: #{name := binary(),
                           pass := binary()}.

-spec authenticate(t_before_auth()) -> t() | {error, invalid_password}.
authenticate(#{name := Name, pass := Pass}) ->
    ValidPassword = <<"alamakota">>,
    case Pass of
        %ValidPassword -> #{name => Name, acl => []};
        ValidPassword -> admin(Name, []);
        _             -> {error, invalid_password}
    end.

-spec admin(binary(), [atom()]) -> t().
admin(Name, ACL) ->
    #{name => Name,
      acl => ACL}.

-spec break_system(t()) -> {ok, broken} | {error, no_access}.
break_system(#{acl := ACL}) ->
    case lists:member(?FUNCTION_NAME, ACL) of
        true  -> {ok, broken};
        false -> {error, no_access}
    end.

test() ->
    %% We have to be more specific: if BeforeAuth is not a return value of a specced function,
    %% Gradualizer doesn't know its type.
    BeforeAuth = before_auth(),
    Admin = authenticate(BeforeAuth),
    AdminWithPower = Admin#{acl := [break_system]},
    [ catch break_system(BeforeAuth),
      break_system(Admin),
      break_system(AdminWithPower) ].

-spec before_auth() -> t_before_auth().
before_auth() ->
    #{name => <<"joe">>,
      pass => <<"alamakota">>}.
