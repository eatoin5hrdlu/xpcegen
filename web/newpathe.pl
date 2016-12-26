:- dynamic webok/0.
:- multifile webok/0.

label(level, Obj) --> [TLevel,' / ',Level,Units],
    { get(Obj,level,Level),
      get(Obj,level_t,TLevel),
      get(Obj,levelUnits,Units) }.

label(od, Obj) --> ['OD',sub(600),'  .',ODTarget,'/.',Turbidity],
    { get(Obj,tturbidity,ODTarget),
      get(Obj,turbidity,Turbidity) }.

label(temperature, Obj) --> ['Temperature ',TargetFmt,' / ',TempFmt],
    {  get(Obj,temperature,Temp),
       DispTemp is float(Temp)/10.0,
       format(atom(TempFmt), '~4g', [DispTemp]),
       get(Obj,ttemperature,TargetTemp),
       DispTarget is float(TargetTemp)/10.0,
       format(atom(TargetFmt), '~4g', [DispTarget]) }.

label(flow, Obj) --> ['Rate ',TFlow,' / ',Flow,Units],
    {  get(Obj,flow,Flow),
       get(Obj,flow_t,TFlow),
       get(Obj,flowUnits,Units) }.

label(supply, Name) --> [Name,br([]),Level,Units],
    { component(Name,supply,Obj),
      get(Obj,levelUnits,Units),
      get(Obj,level,Level) }.

label(cellstat,Name) --> [Name,' ',LLabel,br([]),TLabel,br([]),OLabel],
    { component(Name,cellstat,Obj) },
    label(level,Obj),       [br([])],
    label(temperature,Obj), [br([])],
    label(od, Obj).

label(lagoon,Name) --> [Name,' '],
    {  component(Name,cellstat,Obj),
    label(level,Obj),       [br([])],
    label(temperature,Obj), [br([])],
    label(flow, Obj).

label(sampler,Name) --> [ Name, br([]),
			  'Next Level Reading in ',TimeLeft,'s',br([]),
			  'Next Sample ',TimeAtom ],
    { component(Name,autosampler,Obj),
      get(Obj,timeleft,TimeLeft),
      get(Obj,nextsample,TimeAtom) }.

label(drainage,waste) --> ['Waste'].

web_debug(Req) :-
    nonvar(Req),
    logIP(Req),				
    memberchk(search(Search),Req),
    memberchk(trace='1',Search),
    !,
    ( windows -> 
      write(user_error,'Tracing in Web page generators broken in windows'),nl(user_error)
    ; trace
    ).
web_debug(_).

evostatName(Req,Name) :-
    memberchk(search(List),Req),
    memberchk(evostat(Name),List),
    !.
evostatName(_,Name) :-
     gethostname(Fullname),
     atomic_list_concat([Name|_],'.',Fullname).

backPlate(Name, NamePlate) :-
    concat_atom(['./images/',Name,'.png'],NamePlate).

pathe(Req) :-
  web_debug(Req),
  evostatName(Req,Name),
  backPlate(Name,BackPlate),
  Title = 'Pathe Control Panel',
  semaphore,
  findall(label([id=S],Supply),label(supply,S,Supply,[]),Nutrient_Inducers),
  label(cellstat,_,Cellstat,[]),
  setof(label(id=L,Lagoon),label(lagoon,L,Lagoon,[]),LagoonLabels),
  label(sampler,autosampler,AutoSamplerLabel,[]),
  label(drainage,waste,Waste,[]),
  reply_html_page(
  [title(Title),
   meta(['http-equiv'(refresh),content(5)],[]), % Make it an active page
   script([ language(javascript) ],[])],
% ['location.reload(true)'])],
   body([background(BackPlate)],
	[
          center(a([id=logozone,href('./phagestat.pl')],i(Name))), % click
%	  div(class=phagestat,img(src('./phagestat.png'))),        % hover
	  div(class=supply,Nutrient_Inducers),

	  div(class=cellstat, label([],Cellstat)),
	  div([class=lagoon,width('100%')],LagoonLabels),
	  div(class=autosampler,label([],AutoSamplerLabel)),
          div(class=drainage,label([],Waste)),
          form([class=mod,
                action='./ipathe.pl'],
                input([type=submit,name=submit,value=change]))
        ]
    )% body
  ).

pathe(Request) :-
 errorPage(Request, 'Error creating EvoStat control page').
