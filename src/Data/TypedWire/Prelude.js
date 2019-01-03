// module Data.TypedWire.Prelude

exports.dateToISO = function(date) {
    return date.toISOString();
};

// hacky
exports.dateFromISO = function(string) {
    var d = Date.parse(string);
    if(isNaN(d)) return null;
    return d;
};
