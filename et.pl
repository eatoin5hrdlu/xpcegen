#!/usr/bin/xpce
%:- use_module(library(time)).
:- use_module(library(pce)).
%:- use_module(library(process)).
%:- use_module(library(charsio)).
%:- use_module(library(helpidx)).
%:- use_module(library(lists)).
%:- use_module(library(ctypes)).

%:- use_module(library(time)).
%:- use_module(library(process)).

:- delete_file(iface).

% Generate and XPCE interface to communicate with an Arduino device
%
%    iface(DeviceType, Parameter, Command, ReadWrite).
%
% When the iface/3 term generated by the Arduino in response to 'i' command
% is consulted, this generates the Prolog/XPCE interface.
% iface(lagoon, ebutton,
%      [ int(temperature, t,  0, ro),
%	int(target_temperature, tt, 370, rw),
%	int(turbidity,          b,  ro) ]).
%
% The parent classes contain:
% the variables for the socket, temperatureUnits, turbidityUnits, volumeUnits
% and methods : connect, converse, parse_reply

:- op(1200,xfx,':->'). % Not in PCE mode, so we must define these operators for reading
:- op(1200,xfx,':<-').
:- op(910, xfx,'::').

termout([]).
termout([T|Ts]) :- format('~q.~n', T), termout(Ts).

% Built-in variables for this class, MAC address and Component Name (ID)

term_expansion(iface(Type,PType,Vars), []) :-
        expand_vars(Vars, Declarations, Methods),
	flatten([ (:-style_check(-singleton)),
		  (:- pce_begin_class(Type, PType)),
		  Declarations,
		  ( initialise(Self, Label:[name]) :->
				"Initialise the button and connect to device"::
			send_super(Self, initialise(Label)),
		        send(Self, slot, myname, Label),
			send_super(Self, slot, socket, @nil),
		        bt_device(Label, Mac),
		        send_super(Self, slot, mac, Mac),
		        send_super(Self, connect),
			new(Code2, message(Self, show_conversation)),
			send(Self, recogniser, click_gesture(right,'',single,Code2))),
		  ( update(Self) :->
			  "Load Current Variable values from Device"::
			( get(Self, slot, socket, @nil)
			  -> send(Self,colour,colour(red))
			  ;  send(Self,colour,colour(darkgreen))
			),
		        get(Self, myname, MyName),
			format(user_error,'Updated ~s~n', MyName)),
                  Methods,
		  (:- pce_end_class)], List),
	          tell(iface),
                  termout(List),
                  told,
                  consult(iface).

expand_vars([],[
		variable(myname,name,get,"Component ID"),
		variable(mac,name,get,"Bluetooth MAC Address")
	       ],[
		(get_myname(Self1,Value1:name) :<- get(Self1,myname,Value1)),
		(put_myname(Self1,Value1:name) :<- send(Self1,slot,myname,Value1)),
		(get_mac(Self2,Value2:name) :<- get(Self2,mac,Value2)),
		(put_mac(Self2,Value2:name) :<- send(Self2,slot,mac,Value2))
	       ]).

expand_vars([V|Vs],[D|Ds],[M|Ms]) :-
    expand_var(V, D, M),
    expand_vars(Vs,Ds,Ms).

expand_var(Var,variable(Name,Type,get,Quoted),Methods) :-
    Var =.. [Type, Name, Cmd, Mode],
    concat_atom(["""",Name,""""], Quoted),
    concat_atom(['get_',Name],GetFunctor),
    GetPred =.. [GetFunctor,Self,Value:Type],
    Get = (GetPred :-> send(Self,converse,[Cmd]), parse_reply(Name,Value)),
    ( Mode = ro
      -> Methods = Get
      ;  concat_atom(['put_',Name],PutFunctor),
         PutPred =.. [PutFunctor,Self2,Value2:Type],
	 Put = (PutPred :-> send(Self2,converse,[Cmd,Value2])),
	 Methods = [Get,Put]
    ).

