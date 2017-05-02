function (doc) {
    if (doc.type == "feed") {
	emit({"title": doc.feed.title,
	      "url": doc.feed.url},
	     Date.parse(doc.meta["fetch-ts"]));
    } else {
	emit({"title": doc.meta["source-name"]},
	     Date.parse(doc.meta["fetch-ts"]));
    }
}
