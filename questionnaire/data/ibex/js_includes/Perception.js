function radioBox(name, value) {
  return [["input", {type: "radio", class: "obligatory", name: name, value: value}]];
}

function radioBoxes(name, values) {
  return values.map(function(v){return [["td", {class: "option"}], radioBox(name, v)]});
}

function usage(name, labels, values, labelprefix) {
  labelprefix = labelprefix || "";
  return ["table", ["tr"].concat(labels.map(function(l){return [["td", {class:"option"}]].concat([labelprefix],l)})), ["tr"].concat(radioBoxes(name, values)), ["tr", [["th", {colspan:labels.length}], [["label", {class:"error", for:name}]]]]];
}

define_ibex_controller({
name: "Perception",

jqueryWidget: {
    _init: function () {
        var values = ["onlyout", "moreout", "both", "morein", "onlyin"];
        var vars = [["nobr", "'".concat(this.options.short1, "'")], ["nobr", "'".concat(this.options.short2, "'")]];
        var labels = [["only", vars[0]], ["more", vars[0]], ["both equally"], ["more", vars[1]], ["only", vars[1]]];
        if (this.options.inFirst) {
          values = values.reverse();
          labels = labels.reverse();
        }
        var ageValues = ["out", "in"];
        var ageLabels = vars.map(function(v){return([v, "is older"])});
        if (this.options.inOlderFirst) {
          ageValues = ageValues.reverse();
          ageLabels = ageLabels.reverse();
        }
        var ageQuestions = [["p", "How much do you think", ["i", this.options.oldFirst ? "older speakers" : "younger speakers"], "use either of the variants?"], usage(this.options.oldFirst ? "old" : "young", labels, values, this.options.oldFirst ? "Older speakers use" : "Younger speakers use"), ["p", "How much do you think", ["i", this.options.oldFirst ? "younger speakers" : "older speakers"], "use either of the variants?"], usage(this.options.oldFirst ? "young" : "old", labels, values, this.options.oldFirst ? "Younger speakers use" : "Older speakers use"), ["p", "If you have any other thoughts on the use of ", vars[0], "and", vars[1], " please let us know:"],[["textarea", {name:"comments",rows:5,cols:70}]]];
        this.element.VBox({
            options:     this.options,
            triggers:    [0],
            children:    ["Form", {html: ["div", ["p", "You are probably familiar with these two ways of ".concat(this.options.intro, ":")], [["p", {"class":"stimulus"}], this.options.inFirst ? this.options.long2 : this.options.long1, [["span", {style:"width:6%;display:inline-block"}], " "] , this.options.inFirst ? this.options.long1 : this.options.long2], ["p", "How much do you use either of these variants?"], usage("own_use", labels, values, "I use"), ["p", "How much do you think are people around you using either of the variants?"], usage("people_use", labels, values, "People use"), ["p", "Which of the two variants do you think is", ["i", "older"], "?"], usage("older_variant", ageLabels.concat("People have always used both"), ageValues.concat("same"))].concat(ageQuestions)}]
        });
    }
},

properties: {
    obligatory: ["intro", "long1", "long2", "short1", "short2", "inFirst", "inOlderFirst", "oldFirst"]
}
});
