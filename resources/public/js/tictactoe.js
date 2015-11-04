/** 
  * @desc this class contains all functions for user interaction with tictactoe
  * as well as drawing the board and associated actions.
  * 
  * @author Joshua Cano jcano@dcci.com
*/

var svg = d3.select("body")
.append("svg")
.attr("width", 1200)   // d3 is instantiated Here
.attr("height", 900);

/*
 * The heart of the board, the controller to handle interactions between front-end and server
 */
var ticTacToe = (function() {
	var size = 3; 
	var gameBoard = new Array(size);
	var radius = 50;
	var length = 180;
	var turn = "x";

	for (i=0; i < size; i++)
		gameBoard[i] = new Array(size);

	return {
		gameBoard: function() {return gameBoard;},
		radius: function() {return radius;},
		length: function() {return length;},
		getTurn: function() {return turn;}, 
		reset: function() {
			turn = "x";
			gameBoard = new Array(size);
			for (i=0; i < size; i++)
				gameBoard[i] = new Array(size);
			resetOnServer();
			svg.selectAll("*").remove();
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
		},
		setTurn: function(count) {
			if (count % 2 == 0) turn ="x";
			else turn = "y";
		}
	};  
})(); 


$(document).ready(function() {
	createBoard(svg, 3);
	getBoardFromServer();
});


/*
 * Create board on first load of page
 */
function createBoard(svg, size) {
     var localSize = size - 1;
     var length = ticTacToe.length();
     var xStart = 30;
     var yStart = 420;
     for (i=0; i < (size - 1); i++) {
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

/*
 * Create transparent rectangles which catch user's clicks.
 */
function createRectangles(svg, size) {
     var localSize = size - 1;
     var xStart = 30;
     var gap = 10;
     var length = ticTacToe.length();
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
        poly.classed("game-board", true);
};

function setXClass(polylines) {
        polylines.classed("board-x", true);
};

/*
 * Draw X on Board at specified location
 */
function drawX(xPos, yPos) {
	var lineArray = new Array();
	var lineArray2 = new Array();
        var length = ticTacToe.radius() - 10;
	var matrixSize = 3;
        matrixSize = matrixSize - 1;
        for (i=0; i <= matrixSize; i++) {
           var point = [xPos + (length * i), yPos + (length * i)];
	   lineArray.push(point);
           point = [xPos + ((matrixSize-i) * length), yPos + (length * i)];
	   lineArray2.push(point);
        }
	var points = lineArray.concat(lineArray[1], lineArray2);
	var polyline = svg.append("polyline").attr("points", points);
	setXClass(polyline);
}
/*
 * Draw Circle on Board at specified location
 */
function drawCircle(xPos, yPos) {
     var circle = svg.append("circle").attr({
		cx: xPos,
		cy: yPos,
                class: "board-circle",
	 	r: ticTacToe.radius()});
};

/*
 * Returns Matrix Index of move based on X & Y position of click
 */
function getIndex(xPos, yPos) {
	xIndex = (xPos - 470) / ticTacToe.length();
	yIndex = (yPos - 80) / ticTacToe.length();
        var retMap = {x:xIndex, y:yIndex};
        return retMap;
};

/*
 * Returns Position to start drawing based on x & y index of Matrix
 */
function getPosition(xIndex, yIndex, offset) {
        var length = ticTacToe.length();
	if (offset == undefined) offset = 40;
        var xPos = 470 + offset + (length * xIndex)
        var yPos = 80 + offset + (length * yIndex);
	var retMap = {x:xPos, y:yPos};
        return retMap;
};


/*
 * Handle users Click! Currently only writes an X.
 */
function click() {
  length = 150 / 3;
  var xPos = this.x.baseVal.value + length;
  var yPos = this.y.baseVal.value + length;
  var mapIndex = getIndex(xPos, yPos);
  
  // Ignore the click event if it was suppressed
  if (d3.event.defaultPrevented) return;
  if (ticTacToe.isTaken(mapIndex.x, mapIndex.y)) return;
  if (ticTacToe.computerTurn()) return;

  ticTacToe.storeMove(mapIndex.x, mapIndex.y);

  // Append a new Mark on Board
  drawX(xPos, yPos);
  setMoveOnServer(mapIndex); 
}

/*
 * Draw Circle on page, and store move in ticTacToe Matrix
 */ 
function computerMove (xIndex, yIndex) {
   var mapPos = getPosition(xIndex, yIndex);
   drawCircle(mapPos.x, mapPos.y);
   ticTacToe.storeMove(xIndex, yIndex);
};

/*
 * Handle Winning!
 */
function win(winner) {
   if (winner.x != null) {
   	computerMove(winner.x, winner.y);
   }
   var winDiv = [];
   if (winner.winner == "x" || winner.winner == "y") {
 	winDiv = $('#winImage');
        if (winner.winner == "x") winner.winner = "YOU WON!!!! I AM DEVASTATED.";
	else winner.winner = "The computer won, the dynasty continues!";
   }
   else {
	winDiv = $('#drawImage');
	winner.winner = "The game ended in a draw holmes, SORRY!";
   }
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

/*
 * Reset board on server
 */
function resetOnServer() {
    $.ajax({
        type: "GET",
        url: "/init",
        success: function(data){},
        failure: function(errMsg) { alert(errMsg);}
    });
};

/*
 * Draw moves saved on server for user visualization
 */
function loadBoard(data) {
	var matrix = data.matrix;
	//If there is a Winner Redraw board and set correctly on server.
	if (data.winner) {
             ticTacToe.reset();
	     return;
	}
	var size = matrix.length;
	var turnCount = 0;
	for (var yIndex = 0; yIndex < matrix.length; yIndex++) {
		$.each(matrix[yIndex], function (index, value){
			if (value == "x" || value == "y") {
				var xIndex = index.slice(-1); //get Last Character of value, Change Internal logic to return actual index!
				xIndex = xIndex - 1;
				var position = getPosition(xIndex, yIndex); 
				ticTacToe.storeMove(xIndex, yIndex);
				turnCount++;
				if (value == "x") {
					position = getPosition(xIndex, yIndex, 0);
					drawX(position.x, position.y);
				}
				else drawCircle(position.x, position.y);
			}
		});
	}
	ticTacToe.setTurn(turnCount);
};

/*
 * Asks Server to return current state of Matrix and whether or not there is a winner.
 */
function getBoardFromServer() {
     $.ajax({
	     type: "GET",
	     url: "/matrix",
	     success: function(data) {
		loadBoard(data);
	     },
	     failure: function(errMsg) { alert(errMsg);}
            })
};

/*
 * Sets move on server, display winner if there is one, and display computer's move on screen.
 */
function setMoveOnServer(p) {
    $.ajax({
        type: "POST",
        url: "/store-move",
        data: JSON.stringify(p),
        contentType: "application/json; charset=utf-8",
        dataType: "json",
        success: function(data){
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

