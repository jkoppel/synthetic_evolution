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

function fetchCode() {
    d3.text("../sketch/" + JSON.stringify(path), function (code) {
        editor.setValue(JSON.parse(code));
    });
}

function val(x, d, i) {
    if (typeof(x) == "function") {
        return x(d, i);
    } else {
        return x;
    }
}

function getter(s) {
    return function(d) {
        return d[s];
    };
}

function translate(x, y) {
    return function(sel) {
        sel.attr("transform", function (d, i) {
            return "translate(" + val(x,d,i) + "," + val(y,d,i) + ")";
        });
    }
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
        cursor = cursor[i].subForest;
        fetchCode();
        draw();
     });
     

    g.append("text")
     .attr("y", 10)
     .text(function (d) {
        return JSON.stringify(d.rootLabel);
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
}