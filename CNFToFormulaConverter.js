function discover(element) {
    var elementx = eval(element[0]);
    return loop(elementx)
}

function loop(e) {
    if (e[0] == 'or') {
        var string = [];

        for (i = 1; i < e.length; i++) {
            string.push(loop(e[i]))
        }

        return "(" + string.join(" v ") + ")";
    }

    if (e[0] == 'and') {
        var string = [];
        for (i = 1; i < e.length; i++) {
            string.push(loop(e[i]))
        }

        return "(" + string.join(" ^ ") + ")";
    }

    if (e[0] == 'not') {
        return "~" + loop(e[1]);
    }

    return e[0];
 }