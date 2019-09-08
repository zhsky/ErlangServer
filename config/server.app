{
	application,server,
	[
		{description,"server"},
		{modules,[main]},
		{registered,[server]},
		{applications,[kernel,stdlib,sasl,ranch]},
		{mod,{game_app, []}},
		{start_phases,[]},
		{env,[]}
	]
}.