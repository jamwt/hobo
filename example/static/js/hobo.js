$(function() {
    scan_code();

});


function scan_code() {
    var js_files = {};
    $("code").each(function (i, el) {

        var c = $(el).html();

        var lines = c.split("\n");

        var match = lines[0].match("#![a-z0-9]+");
        if (match) {
            var lang = match[0].substr(2);
            var pre = $(el).parent();
            var url = "/static/js/sh/sh_" + lang + ".min.js";
            pre.addClass("sh_" + lang);
            $(el).html(c.substr(c.indexOf("\n") + 1));
            js_files[url] = 1;
        }
    });

    var load = [];
    for (var v in js_files) {
        load[load.length] = v;
    }

    LazyLoad.js(load, function () {
        sh_highlightDocument();
    });
}
