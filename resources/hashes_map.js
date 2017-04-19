function (doc) {
    if (doc.hash && doc.summary.title) {
	emit(doc.hash, doc.summary.title);
    }
}
