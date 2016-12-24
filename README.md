# xpcegen
Generate an XPCE interface for Arduino/Bluetooth devices from a specification via term_expansion/2

An Arduino XPCE interface class can be generated from a specification of the form:

iface( <class>, <parent-class>, [ <type>( <name>, <cmd>, <read/write>) ... ])

The following creates a  "lagoon" class as an instance of the "ebutton" class.
The class will have six variables and appropriate  get_<name> / put_<name> methods

The "get" method reads the value returned by the Arduino, and
the "put" method will set the variable inside the Arduino 
(although read-only variables will not have a "put_" method).

The XPCE variable slots can be read directly without invoking Arduino communication
and will represent the values obtained during the last communication.

iface( lagoon, ebutton,
      [ int(       temperature,  t, ro),
	int(target_temperature, tt, rw),
	int(              host, v1, rw),
	int(          inducer1, v2, rw),
	int(          inducer2, v3, rw),
	int(          inducer3, v4, rw) ]).


The mechanism can be general, but the current implementation makes several assumptions
about the parent class: Viz.  initialization/update methods will be created and contain
references to the Bluetooth MAC address, socket, and connect, converse, parse_reply,
save, and restore methods which must be provided in the parent class.

iface( cellstat, ebutton,
      [ int(       temperature,  t, ro),
	int(target_temperature, tt, rw),
	int(         turbidity,  b, ro),
	int(  target_turbidity, tb, rw),
	int(        nutrient,    n, rw)	 ]).

iface( supply, ebutton, [ int( volume,  v, rw)]).

iface( sampler, ebutton, 
       [int(    cycle,  c, rw),
	int(  samples,  s, rw),
	int(  lagoon1, d1, rw),
	int(  lagoon2, d2, rw),
	int(  lagoon3, d3, rw),
	int(  lagoon4, d4, rw),
	int( cellstat, d5, rw)  ]).

