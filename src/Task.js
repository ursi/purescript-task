exports.fromPromiseImpl = makeTask => effectUnit => effectPromise => {
	return makeTask(aC => xC => () => {
		effectPromise().then(
			a => aC(a)(),
			x => xC(x)()
		);

		return effectUnit;
	});
};
