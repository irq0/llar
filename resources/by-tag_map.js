function (doc) {
    if (doc.type === 'feed' && doc.meta && doc.meta.tags && Array.isArray(doc.meta.tags)) {
	doc.meta.tags.forEach(function (tag) {
	    emit(tag.toLowerCase(), doc._id);
	});
    }
}
