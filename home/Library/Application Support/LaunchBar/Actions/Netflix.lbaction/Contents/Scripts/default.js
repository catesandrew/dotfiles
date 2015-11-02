function run()
{
    // No argument passed, just open the website:
    LaunchBar.openURL('http://www.netflix.com/');
}

function runWithString(argument)
{
	var url = 'http://dvd.netflix.com/Search?v1=' + encodeURIComponent(argument);
	if ( argument.match(/^http:/))
	{
		url = argument;
	}
    LaunchBar.openURL(url);
}
