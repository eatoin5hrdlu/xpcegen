:- use_module(library(pce)).

% Generate an XPCE/HTML interfaces for an Arduino device
% Arduino responds to 'i' with iface(Class, Parent, Variables)
% E.g. iface(lagoon, ebutton,
%          [int(temperature,   t, ro, "Temperature"),
%           int(ttemperature, tt, rw, "Target Temperature")]).
%
% Our Parent class (ebutton) contains:
% r/w variables: [socket, temperatureUnits, turbidityUnits, volumeUnits]
% and methods : [ connect, converse, parse_reply ]
% Variables mac (address) and Name are created in the new class

:- dynamic changed/2.   % Assertion to tell system to push new
:- multifile changed/2. % values to the Arduino during update

:- op(1200,xfx,':->'). % Define operators for PCE term-expansion
:- op(1200,xfx,':<-').
:- op(910, xfx,'::').

term_expansion(iface(Type,PType,Vars), []) :-
    expand_vars(Vars, VNames, Declarations, Methods, []),
    flatten([ (:-style_check(-singleton)),
       (:- pce_begin_class(Type, PType)),
       Declarations,
       ( initialise(Self, Label:[name]) :->
	 "Initialise button and connect to device"::
	 send_super(Self, initialise(Label)),
	 send(Self, slot, myname, Label),
	 send_super(Self, slot, socket, @nil),
	 bt_device(Label, Mac),
	 send_super(Self, slot, mac, Mac),
	 send_super(Self, connect),
	 new(Code2, message(Self, show_conversation)),
	 send(Self, recogniser,
	      click_gesture(right,'',single,Code2))),
       ( update(US) :->
	 "Push r/w, Get r/o values to/from Device"::
	 ( get(US, slot, socket, @nil)
	 -> send(US,colour,colour(red)) % and no updates
	 ;  send(US,colour,colour(darkgreen)),
	    findall(X,(member(X,VNames),send(US,pull,X)),_X),
	    findall(V,(retract(changed(US,V)),send(US,push,V)),_V)
	 ),
	 get(US, myname, MyName),
	 format(user_error,'Updated ~s~n', MyName)),
       Methods,
       (:- pce_end_class)], List),
    tell(iface),
    maplist(format('~q.~n'),Terms),
    told,
    consult(iface).

expand_vars([],[],[variable(myname,name,both,"Component ID"),
		   variable(mac,name,both,"Bluetooth MAC")]) --> [].

expand_vars([V|Vs], [N|Ns], [D|Ds]) -->
    expand_var(V, N, D),
    expand_vars(Vs, Ns, Ds).

expand_var(Var,Name,variable(Name,Type,both,Doc))  -->
    { Var =.. [Type, Name, Cmd, Mode, Doc] },
    [ (pull(Self,Name:name) :-> "Get value from Arduino"::
                                 send(Self,converse,[Cmd]),
                                 parse_reply(Name,Value),
				 send(Self,Name,Value))],
    pushpred(Mode, Name, Cmd).
    
pushpred(ro,   _,  _) --> [].
pushpred(rw,Name,Cmd) -->
    [ (push(S,Name:name) :-> get(S,Name,V),
                             send(S,converse,[Cmd,V]))].
