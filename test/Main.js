exports.now = () => Date.now();

exports.resolve = () => Promise.resolve(true);

exports.reject = () => Promise.reject(true);
