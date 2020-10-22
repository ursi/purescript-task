exports.now = () => Date.now();

exports.resolve = () => Promise.resolve(true);

exports.reject = () => Promise.reject(true);

exports.waitImpl = ms => () => new Promise(resolve => setTimeout(resolve, ms));

exports.waitRejectImpl = ms => () => new Promise((_, reject) => setTimeout(reject, ms));
