var editor = CodeMirror.fromTextArea(document.getElementById("generated-code"), {
   lineNumbers: true
 , matchBrackets: true
 , mode: "text/x-java"
 });

var data = [];

d3.json("../getsketches/", function (result) {
  data = result;
  preprocess(data);
  draw();
});

function fetchCode(selected) {
    d3.text("../sketch/" + JSON.stringify(selected.path), function (code) {
        editor.setValue(JSON.parse(code));
    });
}

function preprocess(forest, parent, path) {
    if (!path) {
        path = [];
    }

    forest.forEach(function (d, i) {
      var p = path.concat([i]);

      d.selected = false;
      d.parent = parent;
      d.path = p;

      if (parent) {
        d.formatHistory = parent.formatHistory.concat([d.rootLabel.newFormat]);
      } else {
        d.formatHistory = [d.rootLabel.newFormat];
      }

      preprocess(d.subForest, d, p);
    });
}

function makeRects(sel, arr) {
    sel.selectAll("div")
       .data(arr)
       .enter()
       .append("div")
       .text(function(d) {return d.rootLabel.changeDesc;})
       .classed("format-button", true)
       .on("mouseover", function (d) {
          d3.select(this)
            .style("background-color", "#303030")
       })
       .on("mouseout", function (d) {
          if (!d.selected) {
              d3.select(this)
                .style("background-color", "#404040");
          }
       }).on("click", function (d) {
          select(this, d);
       })
       .attr("target_height", function(d) { 
          d.target_height = this.getBoundingClientRect().height;
          return d.target_height;
       })
       .style("height", "0px")
       .transition(3000)
       .style("height", function (d) {
         return d.target_height + "px";
       })
       .transition()
       .style("height", null);
}

function drawFormatHistory(formatHistory) {
    var canvas = d3.select("#format-history-canvas");
    canvas.selectAll("*").remove();
    drawFormat.fileHistory(formatHistory)(canvas);
}

function select(node, d) {
    d3.event.stopPropagation();

    fetchCode(d);
    preprocess(data);

    for(var x = d; x; x = x.parent) {
        x.selected = true;
    }

    drawFormatHistory(d.formatHistory);

    makeRects(d3.select(node), d.subForest);


    d3.select("#format-histories")
      .selectAll("div")
      .filter(function (d) { return !d.selected && d.parent && !d.parent.selected; })
      .transition(3000)
      .style("height", "0px")
      .remove();

    d3.select("#format-histories")
      .selectAll("div")
      .classed("selected", function(d) {return d.selected;});
}


function draw() {
   makeRects(d3.select("#format-histories"), data);
}