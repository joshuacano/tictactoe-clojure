function moveCircle() {
	var circle = d3.selectAll("circle");
	circle.attr("cx", function (d, i) { return i * ((Math.random() % 3 ) * 200) + 30});
}

function setRow(data, index) {
	for (i = 0; i< 3; i++){
		data[i].dy = data[i].dy + (150 * index);
	};
}

function createBoard(svg, size) {
     var localSize = size - 1;
     var length = 180;
     var xStart = 30;
     var yStart = 420;
     for (i=0; i < (size- 1); i++) {
          var xArray = new Array();
          var yArray = new Array();
          var xPos = (yStart + length ) + (i * length);
          var yPos = (xStart + length) + (i * length);
          xArray.push(xPos, xStart);
          xArray.push(xPos, xStart + (length * 3));
          var newPoly = svg.append("polyline").attr("points", xArray );
          boardAttributes(newPoly);
          yArray.push(yStart, yPos);
          yArray.push(yStart + (length * 3), yPos);
          newPoly = svg.append("polyline").attr("points", yArray );
          boardAttributes(newPoly);
     }
     createRectangles(svg, size);
}

function createRectangles(svg, size) {
     var localSize = size - 1;
     var xStart = 30;
     var gap = 10;
     var length = 180;
     var yStart = 420;
     for (i=0; i < size; i++) {
	     for (k=0; k<size; k++) {
		     var newPoly = svg.append("rect").attr({
			x: yStart + (length * k),
			y: xStart + (length * i),
			width: length,
			height: length,
			class: "rect",
			fill: "white",
			opacity: 0});
			newPoly.on("click", click);
		}
	}
};


function boardAttributes(poly) {
        poly.attr("stroke-width", 12);
        poly.attr("fill", "none");
        poly.attr("stroke", "#DA5050");
};

function setPolylineAttr(polylines) {
        polylines.attr("stroke-width", 9);
        polylines.attr("fill", "none");
        polylines.attr("stroke", "steelblue");
};

function drawX(xPos, yPos) {
	var lineArray = new Array();
	var lineArray2 = new Array();
        var length = 40;
	var matrixSize = 3;
        matrixSize = matrixSize - 1;
        for (i=0; i <= matrixSize; i++) {
           var point = [xPos + (length * i), yPos + (length * i)];
	   lineArray.push(point);
           point = [xPos + ((matrixSize-i) * length), yPos + (length * i)];
	   lineArray2.push(point);
        }
        return lineArray.concat(lineArray[1], lineArray2);
}

function drawCircle(xPos, yPos) {
     var circle = svg.append("circle").attr({
		cx: xPos,
		cy: yPos,
                stroke: "steelblue",
                fill: "none",
                "stroke-width": 12,
	 	r: 50});
};


function clone(base) {
	    var newArray = [];
	    for(var i; i < base.length; i++) {
	       newArray[i] = base[i];
				    }
		    return newArray;
};

function dragmove(d) {
  var x = d3.event.x;
  var y = d3.event.y;
  d3.select(this).attr("transform", "translate(" + x + "," + y + ")");
};

function getIndex(xPos, yPos) {
	xIndex = (xPos - 470) / 180;
	yIndex = (yPos - 80) / 180;
        var retMap = {x:xIndex, y:yIndex};
        return retMap;
};

function getPosition(xIndex, yIndex) {
        length = 180;
        var xPos = 470 + 40 + (length * xIndex)
        var yPos = 80 + 40 + (length * yIndex);
	var retMap = {x:xPos, y:yPos};
        return retMap;
};

function click() {
  length = 150 / 3;
  var xLoc = this.x.baseVal.value + length;
  var yLoc = this.y.baseVal.value + length;
  var mapIndex = getIndex(xLoc, yLoc);
  
  // Ignore the click event if it was suppressed
  if (d3.event.defaultPrevented) return;
  if (ticTacToe.isTaken(mapIndex.x, mapIndex.y)) return;
  if (ticTacToe.computerTurn()) return;

  //console.log(ticTacToe.isTaken(mapIndex.x, mapIndex.y));
  ticTacToe.storeMove(mapIndex.x, mapIndex.y);

  // Extract the click location\    
  var point = d3.mouse(this);
  var p = {x: xLoc, y: yLoc};

  // Append a new Mark on Board
  var polyline = svg.append("polyline").attr("points", drawX(p.x, p.y));
  setPolylineAttr(polyline);
  setMoveOnServer(mapIndex); 
}

function computerMove (xIndex, yIndex) {
   var mapPos = getPosition(xIndex, yIndex);
   drawCircle(mapPos.x, mapPos.y);
   ticTacToe.storeMove(xIndex, yIndex);
};

function win(winner) {
   if (winner.x != null) {
   	computerMove(winner.x, winner.y);
   }
   var winDiv = [];
   if (winner.winner == "x" || winner.winner == "y") {
 	winDiv = $('#winImage');
        if (winner.winner == "x") winner.winner = "YOU WON!!!!";
        else winner.winner = "The computer won, the dynasty continues!";
   }
   else winDiv = $('#drawImage');
   $('.winText').html("<h1>" + winner.winner + "</h1>");
   
   var show = function(){
      winDiv.css("display", "block");
      setTimeout(hide, 5000);
   }

   var hide = function() { 
      winDiv.css("display", "none");
      ticTacToe.reset();
   }
   show(); 
};

function resetOnServer() {
    $.ajax({
        type: "GET",
        url: "/init",
        success: function(data){console.log("reset");},
        failure: function(errMsg) { alert(errMsg);}
    });
};

function setMoveOnServer(p) {
    $.ajax({
        type: "POST",
        url: "/store-move",
        data: JSON.stringify(p),
        contentType: "application/json; charset=utf-8",
        dataType: "json",
        success: function(data){
            console.log(data);
        	if (data.winner != null) { //Enter Winning Action
                 win(data);
            } 
            else computerMove(data.x, data.y); //Enter Computer's move on Screen
        },
        failure: function(errMsg) {
            alert(errMsg);
        }
    });
};

var ticTacToe = (function() {
    var size = 3; 
    var gameBoard = new Array(size);
    var turn = "x";

    for (i=0; i < size; i++)
      gameBoard[i] = new Array(size);
 
    return {
        gameBoard: function() {return gameBoard;},
	getTurn: function() {return turn;}, 
        reset: function() {
            turn = "x";
    	    gameBoard = new Array(size);
    	    for (i=0; i < size; i++)
	          gameBoard[i] = new Array(size);
            resetOnServer();
            svg.selectAll("*").remove()
            createBoard(svg, size);
        },
        changeTurn: function() {
	   if (turn == "x") turn = "y";
           else turn = "x";
        },
        storeMove: function(xIndex, yIndex) {
          gameBoard[xIndex][yIndex] = 1; 
          this.changeTurn();
        },
        isTaken: function(xIndex, yIndex) {
	  return gameBoard[xIndex][yIndex] == 1;
	},
        computerTurn: function() {
          return turn == "y";
        }
    };  
})(); 

var svg = d3.select("body")
            .append("svg")
            .attr("width", 1200)   // <-- Here
            .attr("height", 900);
            //.on("click", click);
$(document).ready(function() {
	createBoard(svg, 3);
	var radius = 40;
        console.log(ticTacToe.gameBoard());

});