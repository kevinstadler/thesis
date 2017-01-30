var shuffleSequence = seq(anyType);
var progressBarText = "survey progress";
var pageTitle = "Shetland dialect survey";
//var completionMessage = "";

function randomBoolean() {
        return Math.random() < .5;
}

var incomingFirst = randomBoolean();
var incomingOlder = randomBoolean();
var oldFirst = randomBoolean();

var defaults = [
    "Question", {
        presentHorizontally: true, /* showNumbers: false */
    }
];

var items = [

    ["intro", "Form", {
        html: [["div", {style:"text-align:center"}], ["p", "Hello!"], ["p", "This is a short survey looking at how people use Shetland dialect. It shouldn't take more than 5 minutes to complete. There are no right or wrong answers - don't worry about what seems 'proper', just think about what you would expect to hear or say in relaxed conversation between Shetlanders."], ["p", "This survey will contribute towards research taking place at the University of Edinburgh. Any answers you give will be anonymous and stored securely. In order to participate, you must be over 16; you must have grown up in Shetland, and you must currently live in Shetland."], ["table", ["tr", ["td", [["input", {type:"checkbox", name:"over16", class:"obligatory"}]]], ["td", "I am over 16 years old"]], ["tr", [["td", {colspan:2}], [["label", {class:"error", for:"over16"}]]]], ["tr", ["td", [["input", {type:"checkbox", name:"fromshetland", class:"obligatory"}]]], ["td", "I grew up in Shetland and currently live in Shetland"]], ["tr", [["td", {colspan:2}], [["label", {class:"error", for:"fromshetland"}]]]], ["tr", ["td", [["input", {type:"checkbox", name:"consent", class:"obligatory"}]]], ["td", "I consent to taking part in this research and understand that I can withdraw at any point"]], ["tr", [["td", {colspan:2}], [["label", {class:"error", for:"consent"}]]]]], [["input", {type: "text", style:"display:none", name: "firstvar", value: incomingFirst ? "incoming" : "outgoing"}]],[["input", {type: "text", style:"display:none", name: "firstolder", value: incomingOlder ? "incoming" : "outgoing"}]], [["input", {type: "text", style:"display:none", name: "firstage", value: oldFirst ? "older" : "younger"}]]],
        continueMessage: "Begin survey",
        countsForProgressBar: true
    } ],

    ["imp", "Perception", {intro: "asking somebody to do something", long1: "Mak du dy ain denner!", long2: "Du mak dy ain denner!", short1: "Mak du..", short2: "Du mak..", inFirst: incomingFirst, inOlderFirst:incomingOlder, oldFirst:oldFirst}],
    ["neg", "Perception", {intro: "negating a sentence", long1: "He didna go", long2: "He didnoo go", short1: "didna", short2: "didnoo", inFirst: incomingFirst, inOlderFirst:incomingOlder, oldFirst:oldFirst}],
    ["ynq", "Perception", {intro: "asking somebody a question", long1: "Kens du Sarah?", long2: "Does du ken Sarah?", short1: "Kens du..?", short2: "Does du ken..?", inFirst: incomingFirst, inOlderFirst:incomingOlder, oldFirst:oldFirst}],
    ["whq", "Perception", {intro: "asking somebody a question", long1: "Whit gae du him?", long2: "Whit did du gie him?", short1: "Whit gae du..?", short2: "Whit did du gie..?", inFirst: incomingFirst, inOlderFirst:incomingOlder, oldFirst:oldFirst}],

    ["exit", "Form", {
        html: { include: "exit.html" },
        continueMessage: "Finish survey",
        countsForProgressBar: true,
        validators: {
            age: function (s) { if (s.match(/^\d+$/)) return true; else return "Please enter your age"; }
        } } ]

//    ["send_results", "__SendResults__", { } ],
//    ["done", "Message", {
//        html: "Your data has been recorded, thank you for taking part in this survey!" } ]

];
