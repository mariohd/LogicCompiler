function discover(element) {
    if (element[0].startsWith("[")) {
        return loop(eval(element[0]))
    } else {
        return element[0];
    }
}

function loop(e) {
    if (e[0] == 'or') {
        var s = [];

        for (var i = 1; i < e.length; i++) {
            s.push(loop(e[i]))
        }

        return "(" + s.join(" v ") + ")";
    }

    if (e[0] == 'and') {
        var s = [];
        for (var i = 1; i < e.length; i++) {
            s.push(loop(e[i]))
        }

        return "(" + s.join(" ^ ") + ")";
    }

    if (e[0] == 'not') {
        return "~" + loop(e[1]);
    }

    return e[0];
 }