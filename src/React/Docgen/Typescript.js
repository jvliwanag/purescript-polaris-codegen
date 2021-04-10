const docgen = require("react-docgen-typescript");

exports._parse = docgen.parse;
exports._withCustomConfig = docgen.withCustomConfig;
exports._parserParse = function (parser) {
  return parser.parse;
};
