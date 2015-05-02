
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