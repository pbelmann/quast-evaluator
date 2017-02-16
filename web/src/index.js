'use strict';

var d3 = require("d3"),
    data = require("./data.js"),
    bootstrap = require("bootstrap"),
    extract = function (data, category) {
        return data.map(function (elem) {
            return {
                name: elem["Assembly"],
                category: elem[category]
            };
        }).sort(function (a, b) {
            return b.category - a.category;
        });
    },
    type = "Genome.fraction....",
    prettyType = "Genome Fraction",
    genomeFraction = extract(data.data, type),
    template = require("./file.handlebars"),
    bestGenomeFraction = genomeFraction.splice(0, 3);

require("./base.css");

document.addEventListener("DOMContentLoaded", function () {
    var div = document.createElement('div');
    div.innerHTML = template({others: genomeFraction, best: bestGenomeFraction, type: prettyType});
    document.body.appendChild(div);
});