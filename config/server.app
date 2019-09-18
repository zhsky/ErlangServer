{
	application,server,
	[
		{description,"server"},
		{modules,[main]},
		{registered,[server]},
		{applications,[kernel,stdlib,sasl]},
		{mod,{game_app, []}},
		{start_phases,[]},
		{env,[]}
	]
}.