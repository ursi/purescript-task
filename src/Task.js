exports.fromPromiseImpl = fromForiegn => effectUnit => effectPromise => {
	return fromForiegn(aC => xC => () => {
		effectPromise().then(aC, xC);
		return effectUnit;
	});
};
