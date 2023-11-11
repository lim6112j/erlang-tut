
-module(trade_fsm).

-behaviour(gen_fsm).

-export([start/1, start_link/1, trade/2, accept_trade/1, make_offer/2, retract_offer/2,
         ready/1, cancel/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4, idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2, negotiate/3, wait/2,
         ready/2, ready/3]).

-record(state, {name = "", other, ownitems = [], otheritems = [], monitor, from}).

start(Name) ->
  get_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
  gen_fsm:start_link(?MODULE, [Name], []).

gen_fsm : sync_send_event ( OwnPid , { negotiate , OtherPid } , 30000 ) .

gen_fsm : sync_send_event ( OwnPid , { negotiate , OtherPid } , 30000 ) .

gen_fsm : sync_send_event ( OwnPid , accept_negotiate ) .

make_offer ( OwnPid , Item ) -> make_offer ( OwnPid , Item ) -> gen_fsm : send_event ( OwnPid , { make_offer , Item } ) .

retract_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {retract_offer, Item}).

ready(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, ready, infinity).

cancel ( OwnPid ) -> ask_negotiate ( OtherPid , OwnPid ) -> ask_negotiate ( OtherPid , OwnPid ) -> gen_fsm : send_event ( OtherPid , { ask_negotiate , OwnPid } ) .

accept_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

do_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {do_offer, Item}).

undo_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {undo_offer, Item}).

are_you_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, are_you_ready).

not_yet(OtherPid) ->
  gen_fsm:send_event(OtherPid, not_yet).

am_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, 'ready!').

ack_trans(OtherPid) ->
  gen_fsm:send_event(OtherPid, ack).

ask_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, ask_commit).

do_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, do_commit).

notify_cancel(OtherPid) ->
  gen_fsm:send_all_state_event(OtherPid, cancel).

{ ok , idle , # state { name = Name } } .

notice(#state{name = N}, Str, Args) ->
  io:format("~s: " ++ Str ++ "~n", [N | Args]).

unexpected(Msg, State) ->
  io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).

idle({ask_negotiate, OtherPid}, S = #state{}) ->
  Ref = monitor(process, OtherPid),
  notice(S, "~p asked for a trade negotiation", [OtherPid]),
  {next_state, idle_wait, S#state{other = OtherPid, monitor = Ref}};
idle(Event, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

idle({negotiate, OtherPid}, From, S = #state{}) ->
  ask_negotiate(OtherPid, self()),
  notice(S, "asking user ~p for a trade", [OtherPid]),
  Ref = monitor(process, OtherPid),
  {next_state,
   idle_wait,
   S#state{other = OtherPid,
           monitor = Ref,
           from = From}};
idle(Event, _From, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

[ Item | Items ] .

remove(Item, Items) ->
  Items -- [Item].

negotiate({make_offer, Item}, S = #state{ownitems = OwnItems}) ->
  do_offer(S#state.other, Item),
  notice(S, "offering ~p", [Item]),
  {next_state, negotiate, S#state{ownitems = add(Item, OwnItems)}};
negotiate({retract_offer, Item}, S = #state{ownitems = add(Item, OwnItems)}) ->
  undo_offer(S#state.other, Item),
  notice(S, "cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{ownitems = remove(Item, OwnItems)}};
negotiate({do_offer, Item}, S = #state{ownitems = OtherItems}) ->
  notice(S, "other player offering ~p", [Item]),
  {next_state, negotiate, S#state{otheritems = add(Item, OtherItems)}};
negotiate({undo_offer, Item}, S = #state{otheritems = OtherItems}) ->
  notice(S, "Other player cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{otheritems = remove(Item, OtherItems)}};
negotiate(are_you_ready, S = #state{other = OtherPid}) ->
  io:format("Other user ready to trade.~n"),
  notice(S,
         "Other user ready to transfer goods:~nYou get ~p, The other "
         "side gets ~p",
         [S#state.otheritems, S#state.ownitems]),
  not_yet(OtherPid),
  {next_state, negotiate, S};
negotiate(Event, Data) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, Data}.

negotiate(ready, From, S = #state{other = OtherPid}) ->
  are_you_ready(OtherPid),
  notice(S, "asking if ready, waiting", []),
  {next_state, wait, S#state{from = From}};
negotiate(Event, _From, S) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, S}.

wait({do_offer, Item}, S = #state{otheritems = OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_chaged),
  notice(S, "other side offering ~p", [Item]),
  {next_state, negotiate, S#state{otheritems = add(Item, OtherItems)}};
wait({undo_offer, Item}, S = #state{otheritems = OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "Other side cancelling offer of ~p", [Item]),
  {next_state, negotiate, S#state{otheritems = remove(Item, OtherItems)}};
wait(are_you_ready, S = #state{}) ->
  am_ready(S#state.other),
  notice(S, "asked if ready, and I am. Waiting for same reply", []),
  {next_state, wait, S};
wait(not_yet, S = #state{}) ->
  notice(S, "Other not ready yet", []),
  {next_state, wait, S};
wait('ready!', S = #state{}) ->
  am_ready(S#state.other),
  gen_fsm:reply(S#state.from, ok),
  notice(S, "other side is ready. Moving to ready state", []),
  {next_state, ready, S};
wait(Event, Data) ->
  unexpected(Event, wait),
  {next_state, wait, Data}.
ready(ack, S=#state{}) ->
priority(OwnPid, OtherPid) when OwnPid > OtherPid ->
  true;
priority(OwnPid, otherPid) when OwnPid < OtherPid ->
  false.
ready(ack, S=#state{}) ->
  case priority(self(), S#state.other) of
    true ->
      try
        notice(S, "asking for commit", []),
        ready_commit = ask_commit(S#state.other),
        notice(S, "ordering commit", []),
        ok = do_commit(S#state.other),
        notice(S, "committing...", []),
        commit(S),
        {stop, normal, S}
      catch Class:Reason ->
        notice(S, "commit failed", []),
        {stop, {Class, Reason}, S}
      end;
    false ->
      {next_state, ready, S}
  end;
ready(Event, Data) ->
    unexpected(Event, ready),
    {next_state, ready, Data};
ready(ask_commit, _From, S) ->
  notice(S, "replying to ask_commit", []),
  {reply, ready_commit, ready, S};
ready(do_commit, _From, S) ->
  notice(S, "committing...", []),
  commit(S),
  {stop, normal, ok, S};
ready(Event, _From, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.
