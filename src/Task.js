export const fromPromiseImpl = fromForiegn => effectUnit => effectPromise => {
	return fromForiegn(aC => xC => () => {
		effectPromise().then(aC, xC);
		return effectUnit;
	});
};

export const toPromiseImpl = f => () => new Promise(f);
