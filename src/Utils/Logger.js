exports.mylog = str => val => (console.log(str), val);
exports.error = str  => () => {
    console.log(str);
    throw str;
}
