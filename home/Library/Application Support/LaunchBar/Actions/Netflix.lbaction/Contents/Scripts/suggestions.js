function sleep(milliseconds) {
	var end = new Date().getTime() + milliseconds;
	while (end > new Date().getTime())
	{
		continue;
	}
}

function runWithString(argument) {

	sleep(300); // Slow our requests to be nice to Netflix

	var result = HTTP.getJSON("http://dvd.netflix.com/JSON/AutoCompleteSearch?n=10&type=grouped&loc=1&v=3&prefix=" + encodeURIComponent(argument), 3);

	if (result === undefined)
	{
		return [ {
			title: "Error!",
			icon: "at.obdev.LaunchBar:InfoTemplate"
		} ];
	}

	var groups = result.data.groups;
	var items = [];

	for (var i = 0; i < groups.length; i++) {
		var group = groups[i];

		for (var i = 0; i < group.items.length; i++)
		{
			items.push( group.items[i] );
		};
	};
	var suggestions = [];

	for (var i = 0; i < items.length; i++) {
		var item = items[i];

		suggestions.push({
			title: item.title,
			url: "http://www.netflix.com/WiMovie/" + item.id,
			icon: "at.obdev.LaunchBar:MoviesTemplate"
		});
	};
	File.writeJSON(suggestions, "/tmp/blarp.txt");

	if (suggestions.length === 0)
	{
		return [ {
			title: "No results found for " + argument,
			icon: "at.obdev.LaunchBar:InfoTemplate"
		} ];
	}

	return suggestions;
}
