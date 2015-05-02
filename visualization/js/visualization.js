var editor = CodeMirror.fromTextArea(document.getElementById("generated-code"), {
   lineNumbers: true
 , matchBrackets: true
 , mode: "text/x-java"
 });

var data = [];
var cursor = {};

d3.json("../getsketches/", function (result) {
  //data = {rootLabel: [], subForest: result};
  data = result;
  cursor = data;
  draw();
});

var svgWidth = 1000;
var svgHeight = 800;

var height = 50;
var padding = 10;
var width = 50;

var cursor = [];
var path = [];
var formatHistory = [];

function fetchCode() {
    d3.text("../sketch/" + JSON.stringify(path), function (code) {
        editor.setValue(JSON.parse(code));
    });
}

function makeRect(sel) {

    var heightFn = function(d,i) {return i*(height+padding);};

    var g = sel.append("g")
               .call(translate(0, heightFn));



    g.append("rect")
     .attr("class", "format-rect")
     .attr("height", height)
     .attr("width", width)
     .on("mouseover", function(d) {
         d3.select(this).style("fill", "red");
     })
     .on("mouseout", function(d) {
         d3.select(this).style("fill", "blue");
     })
     .on("click", function(d, i) {

        /*if (d.parent) {
            d.parent.subForest.forEach(function (e) { e.selected = false; })
        }*/

        //d.selected = true;
        path.push(i);
        formatHistory.push(cursor[i].rootLabel.newFormat);
        cursor = cursor[i].subForest;
        fetchCode();
        draw();
     });
     

    g.append("text")
     .attr("y", 10)
     .text(function (d) {
        return d.rootLabel.changeDesc;
     });

}



function draw() {
    d3.select("svg").remove();

    /*var treeLayout = d3.layout.tree()
                              .size([svgWidth / 2, svgHeight / 2])
                              .children(function (d) {
                                if (d.selected) {
                                    return d.subForest;
                                } else {
                                    return null;
                                }
                               });

    var nodes = treeLayout.nodes(data);*/

    var svg = d3.select("#format-histories")
                .append("svg")
                .attr("width", svgWidth)
                .attr("height", svgHeight);

    var sel = svg.selectAll("rect")
        .data(cursor)
        .enter();

    makeRect(sel);

    drawFormat.fileHistory(formatHistory)(d3.select("svg")).call(translate(20, 100));
}