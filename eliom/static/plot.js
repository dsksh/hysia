function plot(holderId, data) {
alert(data.length);

	var arr = [];
	for (var i = 0, j = 0; i < data.length; i++) {
		var d = data[i];
		if (d["state"]["kind"] == 'pped') continue;

		arr[j] = [[],[]];

		var ti = d["time"];
		arr[j][0] = (ti[0]+ti[1])/2;
		var y1i = d["state"]["value"][1];
		arr[j][1] = (y1i[0]+y1i[1])/2;
		j++;
	}

	g = new Dygraph(document.getElementById(holderId), arr, {width:1000, height:350});
}

function readData(holderId, filename) {
alert(holderId);
	var httpObj = new XMLHttpRequest();
	httpObj.open("GET", filename, true);
	httpObj.onreadystatechange = function() {
		if (httpObj.readyState == 4) {
			//var e = document.getElementById(holderId);
			//e.value = httpObj.responseText;
alert(httpObj.responseText);
			data = eval("("+httpObj.responseText+")");
			plot(holderId, data);
		}
    }
    httpObj.send(null);
}
