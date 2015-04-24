function plot(holderId, yvar, data, tsim) {
//alert(data.length);

	var arr = [];
	for (var i = 0, j = 0; i < data.length; i++) {

		var d = data[i];
		if (d.state === undefined) continue;
		if (d["state"]["kind"] == 'pped') continue;

		var ti = d["time"];
		if (ti[0] > tsim) break;

		arr[j] = [];

		var y1i = d["state"]["value"][yvar];
		//arr[j][0] = (ti[0]+ti[1])/2;
		//arr[j][1] = (y1i[0]+y1i[1])/2;
		arr[j][0] = ti[0];
		arr[j][1] = y1i[0];
		arr[j][2] = y1i[1];
		j++;

		arr[j] = [];
		arr[j][0] = ti[1];
		arr[j][1] = y1i[0];
		arr[j][2] = y1i[1];
		j++;
	}

	g = new Dygraph(document.getElementById(holderId), arr, 
	  {width:1000, height:350, colors:['#0b615e','#088a85']});
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
