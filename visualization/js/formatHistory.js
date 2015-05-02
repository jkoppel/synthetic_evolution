var drawFormat = {};


drawFormat.fileField = function(outerField) {
    var fieldID = outerField[0];

    var height = 30;
    var unitWidth = 20;

    function fieldColor() {
        switch (fieldID) {
            case 1: return "green";
            case 2: return "blue";
            case 3: return "yellow";
        }
    }

    function rawField(field) {
        function fieldName() {
            return "field" + fieldID;
        }

        function my (sel) {
            var g = sel.append("g");

            g.append("rect")
             .style("fill", fieldColor())
             .attr("width", unitWidth*my.len())
             .attr("height", height)
             .attr("class", "field-rect");

            g.append("text")
             .text(fieldName())
             .style("text-anchor", "middle")
             .attr("x", unitWidth*my.len() / 2)
             .attr("y", (height/2)+5);

            return g;
        }

        my.width = function() {
            return my.len() * unitWidth;
        };


        my.len = function() {
            return 4;
        }

        return my;
    }

    function attributeCodeField(field) {
        var innerField = drawFormat.fileField([field[0], field[1].AttributeCode]);

        var attributeCodeLength = 4;

        function my (sel) {

            var g = sel.append("g");

            g.append("rect")
             .style("fill", fieldColor())
             .attr("width", unitWidth * attributeCodeLength)
             .attr("height", height)
             .attr("class", "field-rect");

            g.append("text")
             .text("" + fieldID)
             .style("text-anchor", "middle")
             .style("background-color", "orange")
             .attr("x", unitWidth * attributeCodeLength / 2)
             .attr("y", (height/2)+5);

            var innerG = g.append("g")
                          .call(translate(my.width() - innerField.width(), 0));

            innerField(innerG);

            return g;
        }

        my.len = function() {
            return attributeCodeLength + innerField.len();
        };

        my.width = function() {
            return my.len() * unitWidth;
        };

        return my;
    }

    if (outerField[1] == "Raw") {
        return rawField(outerField);
    } else {
        return attributeCodeField(outerField);
    }
}

drawFormat.fileHistory = function(formatHistory) {
    var xOff = 0;
    var yOff = 0;
    var spacing = 50;

    function my(sel) {
        var y = yOff;

        var g = sel.append("g");

        formatHistory.forEach(function(curFormat) {
            var x = xOff;
            curFormat.forEach(function (f) {
                var fld = drawFormat.fileField(f);
                fld(g).call(translate(x, y));
                x += fld.width();
            });
            y += spacing;
        });

        return g;
    }

    return my;
}