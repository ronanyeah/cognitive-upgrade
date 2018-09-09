const { writeFileSync } = require("fs");
const cb = require("./cb.json", "utf8");

const txt = `module Cb exposing (cb)


cb : String
cb =
    """
${JSON.stringify(cb)}
"""
`;

writeFileSync("./src/Cb.elm", txt);
