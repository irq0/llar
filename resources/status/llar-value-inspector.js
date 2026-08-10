(function (global) {
  "use strict";

  function element(tag, className, text) {
    var node = document.createElement(tag);
    if (className) node.className = className;
    if (text !== undefined && text !== null) node.textContent = text;
    return node;
  }

  function shortType(type) {
    if (!type) return "nil";
    var parts = String(type).split(/[.$]/);
    return parts[parts.length - 1] || type;
  }

  function typeBadge(node, state) {
    if (!state.showTypes || !node["runtime-type"]) return null;
    var badge = element(
      "span",
      "clojure-inspector-type",
      shortType(node["runtime-type"]),
    );
    badge.title = node["runtime-type"];
    return badge;
  }

  function collectionSummary(node) {
    var populated = node.children && node.children.length;
    var shape =
      node.open + (populated || node.truncated ? "…" : "") + node.close;
    var unit = node.kind === "map" ? "entry" : "item";
    var pluralUnit = node.kind === "map" ? "entries" : "items";
    var count = node["count-known"]
      ? node.count.toLocaleString() +
        (node.count === 1 ? " " + unit : " " + pluralUnit)
      : "count unknown";
    if (node.truncated) {
      if (node["omitted-count"] !== null && node["omitted-count"] !== undefined)
        count += " · " + node["omitted-count"].toLocaleString() + " omitted";
      else count += " · truncated";
    }
    return shape + "  " + count;
  }

  function nodeText(node) {
    if (!node) return "nil";
    if (node.kind === "scalar" || node.kind === "truncated") {
      var printed = node.printed || (node.truncated ? "…" : "");
      return node["semantic-type"] === "string" && printed.length > 180
        ? printed.slice(0, 179) + "…"
        : printed;
    }
    return collectionSummary(node);
  }

  function nodeToEdn(node) {
    if (!node) return "nil";
    if (node.kind === "scalar" || node.kind === "truncated")
      return node.printed;
    var values;
    if (node.kind === "map") {
      values = (node.children || []).map(function (entry) {
        return nodeToEdn(entry.key) + " " + nodeToEdn(entry.value);
      });
    } else {
      values = (node.children || []).map(nodeToEdn);
    }
    if (node.truncated) values.push("…");
    return (
      node.open + values.join(node.kind === "map" ? ", " : " ") + node.close
    );
  }

  function keyIdentity(node) {
    if (!node || node.kind !== "scalar" || node.truncated) return null;
    var supported = [
      "keyword",
      "string",
      "symbol",
      "character",
      "boolean",
      "integer",
      "number",
      "ratio",
      "uuid",
      "nil",
    ];
    if (!supported.includes(node["semantic-type"])) return null;
    return [node["runtime-type"], node["semantic-type"], node.printed].join(
      "\u0000",
    );
  }

  function pathSegment(keyNode, index, mapEntry) {
    if (!mapEntry)
      return {
        kind: "index",
        index: index,
        display: "[" + index + "]",
        safe: true,
      };
    return {
      kind: "key",
      node: keyNode,
      display:
        keyNode && keyNode.printed ? keyNode.printed : "[key " + index + "]",
      safe: Boolean(keyNode && keyNode.printed && !keyNode.truncated),
    };
  }

  function pathId(path) {
    return path
      .map(function (segment) {
        return segment.kind === "index"
          ? "i:" + segment.index
          : "k:" + (keyIdentity(segment.node) || segment.display);
      })
      .join("/");
  }

  function pathToEdn(path) {
    return (
      "[" +
      path
        .map(function (segment) {
          return segment.kind === "index"
            ? String(segment.index)
            : segment.display;
        })
        .join(" ") +
      "]"
    );
  }

  function pathDisplay(rootLabel, path) {
    return [rootLabel]
      .concat(
        path.map(function (segment) {
          return segment.display;
        }),
      )
      .join(" › ");
  }

  function nodeAtPath(node, printedPath) {
    return (printedPath || []).reduce(function (current, segment) {
      if (!current) return null;
      if (typeof segment === "number" && current.kind === "collection")
        return (current.children || [])[segment] || null;
      if (current.kind !== "map") return null;
      var entry = (current.children || []).find(function (candidate) {
        return candidate.key && candidate.key.printed === segment;
      });
      return entry ? entry.value : null;
    }, node);
  }

  function nodeAtSegments(node, path) {
    return (path || []).reduce(function (current, segment) {
      if (!current) return null;
      if (segment.kind === "index")
        return current.kind === "collection"
          ? (current.children || [])[segment.index] || null
          : null;
      if (current.kind !== "map") return null;
      var identity = keyIdentity(segment.node);
      var entry = (current.children || []).find(function (candidate) {
        return identity
          ? keyIdentity(candidate.key) === identity
          : candidate.key && candidate.key.printed === segment.display;
      });
      return entry ? entry.value : null;
    }, node);
  }

  function truncationLabel(node) {
    switch (node["truncation-reason"]) {
      case "depth":
        return "maximum depth reached";
      case "nodes":
        return "display node budget reached";
      case "characters":
        return "display character budget reached";
      case "string-length":
        return "string capture limit reached";
      case "printed-length":
        return "printed representation limit reached";
      default:
        return "value truncated";
    }
  }

  function matches(node, query) {
    if (!query) return true;
    var needle = query.toLocaleLowerCase();
    if (
      [
        node.printed,
        nodeText(node),
        node["runtime-type"],
        node["semantic-type"],
      ]
        .filter(Boolean)
        .some(function (value) {
          return String(value).toLocaleLowerCase().includes(needle);
        })
    )
      return true;
    if (node.metadata && matches(node.metadata, query)) return true;
    if (node.kind === "map") {
      return (node.children || []).some(function (entry) {
        return matches(entry.key, query) || matches(entry.value, query);
      });
    }
    return (node.children || []).some(function (child) {
      return matches(child, query);
    });
  }

  function selectNode(node, path, state, selectedElement) {
    state.container
      .querySelectorAll(".clojure-inspector-inline-selected")
      .forEach(function (element) {
        element.classList.remove("clojure-inspector-inline-selected");
      });
    if (selectedElement)
      selectedElement.classList.add("clojure-inspector-inline-selected");
    state.selectedNode = node;
    state.selectedPath = path;
    state.selectedPathId = pathId(path);
    updateSelectionDisplay(state);
  }

  function updateSelectionDisplay(state) {
    state.selection.textContent = "";
    var root = currentRoot(state);
    var path = state.selectedPath || [];
    var node = state.selectedNode || (root && root.node);
    var pathCode = element(
      "code",
      "clojure-inspector-selected-path",
      pathDisplay(root ? root.label : "Value", path),
    );
    var description = element(
      "span",
      "clojure-inspector-selected-type",
      node["runtime-type"] || node["semantic-type"] || "nil",
    );
    state.selection.append(pathCode, description);
    state.copyPath.disabled =
      !node ||
      path.some(function (segment) {
        return !segment.safe;
      });
  }

  function inlineValue(node, state, path, keyMode) {
    var wrapper = element("span", "clojure-inspector-inline");
    if (state.selectedPathId === pathId(path))
      wrapper.classList.add("clojure-inspector-inline-selected");
    var code = element(
      "code",
      "clojure-inspector-token clojure-inspector-token-" +
        (node["semantic-type"] || node.kind),
      nodeText(node),
    );
    wrapper.append(code);
    if (!keyMode) {
      var badge = typeBadge(node, state);
      if (badge) wrapper.append(badge);
    }
    if (node.length && node.length > 160) {
      wrapper.append(
        element(
          "span",
          "clojure-inspector-length",
          node["truncation-reason"] === "string-length"
            ? (node["captured-length"] || 0).toLocaleString() +
                " of " +
                node.length.toLocaleString() +
                " chars"
            : node.length.toLocaleString() + " chars",
        ),
      );
    }
    if (node.truncated) {
      var truncation = element(
        "span",
        "clojure-inspector-truncated",
        "truncated",
      );
      truncation.title = truncationLabel(node);
      wrapper.append(truncation);
    }
    if (!state.compact)
      wrapper.addEventListener("click", function (event) {
        event.stopPropagation();
        selectNode(node, path, state, wrapper);
      });
    return wrapper;
  }

  function treeLine(keyNode, valueNode, state, path, index, mapEntry) {
    var line = element("span", "clojure-inspector-line");
    line.append(element("span", "clojure-inspector-marker"));
    var key = element("span", "clojure-inspector-key");
    if (keyNode) key.append(inlineValue(keyNode, state, path, true));
    else if (!mapEntry && index !== null)
      key.append(element("code", "clojure-inspector-index", "[" + index + "]"));
    line.append(key);
    var value = element("span", "clojure-inspector-value");
    value.append(inlineValue(valueNode, state, path, false));
    line.append(value);
    return line;
  }

  function renderTreeNode(node, state, depth, path, keyNode, index, mapEntry) {
    if (
      !matches(node, state.query) &&
      !(keyNode && matches(keyNode, state.query))
    )
      return null;

    if (node.kind === "scalar" || node.kind === "truncated") {
      var leaf = element(
        "div",
        "clojure-inspector-entry clojure-inspector-leaf",
      );
      var longString = node["semantic-type"] === "string" && node.length > 160;
      if (longString) {
        var longDetails = element(
          "details",
          "clojure-inspector-long-value clojure-inspector-entry",
        );
        var longSummary = element("summary");
        longSummary.append(
          treeLine(keyNode, node, state, path, index, mapEntry),
        );
        longDetails.append(longSummary);
        var captured = element(
          "pre",
          "clojure-inspector-long-value-body",
          node.printed,
        );
        if (node.truncated)
          captured.append(
            element(
              "span",
              "clojure-inspector-long-value-warning",
              "\n… " + truncationLabel(node) + " …",
            ),
          );
        longDetails.append(captured);
        leaf.append(longDetails);
      } else {
        leaf.append(treeLine(keyNode, node, state, path, index, mapEntry));
      }
      return leaf;
    }

    var details = element(
      "details",
      "clojure-inspector-entry clojure-inspector-collection" +
        (depth === 0 ? " clojure-inspector-root" : ""),
    );
    var expansionKey = currentRoot(state).id + ":" + pathId(path);
    details.open = state.query
      ? true
      : state.collapsedPaths.has(expansionKey)
        ? false
        : state.expandedPaths.has(expansionKey) || depth < state.initialDepth;
    var summary = element("summary");
    summary.append(treeLine(keyNode, node, state, path, index, mapEntry));
    details.append(summary);
    var children = element("div", "clojure-inspector-children");
    details.append(children);
    var rendered = false;

    function renderChildren() {
      if (rendered) return;
      rendered = true;
      if (node.kind === "map") {
        (node.children || []).forEach(function (entry, childIndex) {
          var segment = pathSegment(entry.key, childIndex, true);
          var childPath = path.concat([segment]);
          var child = renderTreeNode(
            entry.value,
            state,
            depth + 1,
            childPath,
            entry.key,
            childIndex,
            true,
          );
          if (child) children.append(child);
        });
      } else {
        (node.children || []).forEach(function (entry, childIndex) {
          var childPath = path.concat([pathSegment(null, childIndex, false)]);
          var child = renderTreeNode(
            entry,
            state,
            depth + 1,
            childPath,
            null,
            childIndex,
            false,
          );
          if (child) children.append(child);
        });
      }
      if (node.metadata) {
        var metadata = renderTreeNode(
          node.metadata,
          state,
          depth + 1,
          path.concat([
            {
              kind: "key",
              node: {
                kind: "scalar",
                "semantic-type": "symbol",
                "runtime-type": "clojure.lang.Symbol",
                printed: "^meta",
              },
              display: "^meta",
              safe: false,
            },
          ]),
          {
            kind: "scalar",
            "semantic-type": "symbol",
            "runtime-type": "clojure.lang.Symbol",
            printed: "^meta",
          },
          null,
          true,
        );
        if (metadata) children.append(metadata);
      }
      if (node.truncated) {
        var omitted = node["omitted-count"];
        children.append(
          element(
            "div",
            "clojure-inspector-omitted",
            omitted !== null && omitted !== undefined
              ? "… " +
                  omitted.toLocaleString() +
                  " more · " +
                  truncationLabel(node)
              : "… " + truncationLabel(node),
          ),
        );
      }
    }

    details.addEventListener("toggle", function () {
      if (details.open) {
        state.expandedPaths.add(expansionKey);
        state.collapsedPaths.delete(expansionKey);
        renderChildren();
      } else {
        state.collapsedPaths.add(expansionKey);
        state.expandedPaths.delete(expansionKey);
      }
    });
    if (details.open) renderChildren();
    return details;
  }

  function tableSchema(node) {
    if (
      !node ||
      node.kind !== "collection" ||
      node.truncated ||
      !node.children ||
      node.children.length === 0 ||
      !node.children.every(function (child) {
        return child.kind === "map" && !child.truncated;
      })
    )
      return null;

    var schemas = node.children.map(function (row) {
      var seen = new Set();
      var columns = [];
      for (var index = 0; index < (row.children || []).length; index += 1) {
        var entry = row.children[index];
        var id = keyIdentity(entry.key);
        if (!id || seen.has(id)) return null;
        seen.add(id);
        columns.push({ id: id, key: entry.key });
      }
      return columns;
    });
    if (
      schemas.some(function (schema) {
        return !schema;
      })
    )
      return null;
    var columns = schemas[0];
    if (columns.length === 0 || columns.length > 30) return null;
    var expected = new Set(
      columns.map(function (column) {
        return column.id;
      }),
    );
    var regular = schemas.every(function (schema) {
      return (
        schema.length === columns.length &&
        schema.every(function (column) {
          return expected.has(column.id);
        })
      );
    });
    return regular ? columns : null;
  }

  function renderTable(node, state, rootPath, columns, rootLabel) {
    var rows = node.children
      .map(function (row, index) {
        return { node: row, index: index };
      })
      .filter(function (row) {
        return matches(row.node, state.query);
      });
    var wrapper = element(
      "div",
      "table-responsive clojure-inspector-table-wrap",
    );
    if (!rows.length) {
      wrapper.append(
        element(
          "div",
          "clojure-inspector-empty",
          "No rows match “" + state.query + "”.",
        ),
      );
      return wrapper;
    }
    var table = element(
      "table",
      "table table-sm table-hover clojure-inspector-table",
    );
    table.append(element("caption", "visually-hidden", rootLabel));
    var thead = element("thead");
    var header = element("tr");
    var indexHeader = element("th", null, "#");
    indexHeader.scope = "col";
    header.append(indexHeader);
    columns.forEach(function (column) {
      var cell = element("th");
      cell.scope = "col";
      cell.append(
        inlineValue(
          column.key,
          state,
          rootPath.concat([pathSegment(column.key, 0, true)]),
          true,
        ),
      );
      header.append(cell);
    });
    thead.append(header);
    table.append(thead);
    var tbody = element("tbody");
    rows.forEach(function (row) {
      var rowIndex = row.index;
      var tr = element("tr");
      var rowHeader = element(
        "th",
        "clojure-inspector-index",
        "[" + rowIndex + "]",
      );
      rowHeader.scope = "row";
      tr.append(rowHeader);
      var entries = new Map(
        (row.node.children || []).map(function (entry) {
          return [keyIdentity(entry.key), entry.value];
        }),
      );
      columns.forEach(function (column) {
        var td = element("td");
        var value = entries.get(column.id);
        if (value) {
          var cellPath = rootPath.concat([
            pathSegment(null, rowIndex, false),
            pathSegment(column.key, 0, true),
          ]);
          if (value.kind === "scalar" || value.kind === "truncated") {
            td.append(inlineValue(value, state, cellPath, false));
          } else {
            var details = element("details", "clojure-inspector-table-detail");
            var summary = element("summary");
            summary.append(element("span", "clojure-inspector-table-marker"));
            summary.append(inlineValue(value, state, cellPath, false));
            details.append(summary);
            details.addEventListener("toggle", function () {
              if (details.open && details.childElementCount === 1)
                details.append(
                  renderTreeNode(value, state, 0, cellPath, null, null, false),
                );
            });
            td.append(details);
          }
        }
        tr.append(td);
      });
      tbody.append(tr);
    });
    table.append(tbody);
    wrapper.append(table);
    return wrapper;
  }

  function currentRoot(state) {
    return (
      state.payload.roots.find(function (root) {
        return root.id === state.rootSelect.value;
      }) || state.payload.roots[0]
    );
  }

  function render(state) {
    var root = currentRoot(state);
    state.content.textContent = "";
    if (!root) {
      state.content.append(
        element("div", "text-muted", "No value captured yet."),
      );
      return;
    }
    var columns = tableSchema(root.node);
    var canTable = Boolean(columns);
    state.viewSelect.querySelector('option[value="table"]').disabled =
      !canTable;
    state.viewSelect.querySelector('option[value="table"]').title = canTable
      ? "Regular collection of maps"
      : "Available for regular collections with no more than 30 scalar-keyed fields";
    if (!canTable && state.viewSelect.value === "table")
      state.viewSelect.value = "tree";
    state.viewByRoot[root.id] = state.viewSelect.value;
    var rootPath = [];
    var rendered;
    if (state.viewSelect.value === "table" && canTable)
      rendered = renderTable(root.node, state, rootPath, columns, root.label);
    else
      rendered = renderTreeNode(
        root.node,
        state,
        0,
        rootPath,
        null,
        null,
        false,
      );
    if (rendered) state.content.append(rendered);
    else
      state.content.append(
        element(
          "div",
          "clojure-inspector-empty",
          "No values match “" + state.query + "”.",
        ),
      );
    if (state.compact) {
      state.selectedNode = null;
      state.selectedPath = [];
      state.selectedPathId = null;
    } else if (state.selectedRootId !== root.id || !state.selectedNode) {
      state.selectedRootId = root.id;
      selectNode(root.node, rootPath, state, null);
    } else {
      updateSelectionDisplay(state);
    }
  }

  function copyText(value) {
    if (navigator.clipboard && navigator.clipboard.writeText)
      return navigator.clipboard.writeText(value);
    var area = element("textarea");
    area.value = value;
    area.style.position = "fixed";
    area.style.opacity = "0";
    document.body.append(area);
    area.select();
    var copied = document.execCommand("copy");
    area.remove();
    return copied
      ? Promise.resolve()
      : Promise.reject(new Error("Copy was rejected"));
  }

  function announceCopy(state, success, message) {
    state.copyStatus.textContent = message;
    state.copyPath.textContent = success ? "Copied path" : "Copy failed";
    global.setTimeout(function () {
      state.copyPath.textContent = "Copy get-in path";
      state.copyStatus.textContent = "";
    }, 1800);
  }

  function mount(container, payload, options) {
    options = options || {};
    var compact = options.variant === "compact";
    var previous = container.__llarValueInspector;
    container.textContent = "";
    container.classList.add("clojure-value-inspector");
    container.classList.toggle("clojure-value-inspector-compact", compact);
    var toolbar = element("div", "clojure-inspector-toolbar");
    var rootSelect = element("select", "form-select form-select-sm");
    rootSelect.setAttribute("aria-label", "Inspected value root");
    (payload.roots || []).forEach(function (root) {
      var option = element("option", null, root.label);
      option.value = root.id;
      rootSelect.append(option);
    });
    if (options.initialRoot) rootSelect.value = options.initialRoot;
    else if (previous && previous.activeRootId)
      rootSelect.value = previous.activeRootId;
    if (!rootSelect.value && payload.roots && payload.roots.length)
      rootSelect.value = payload.roots[0].id;
    var viewSelect = element("select", "form-select form-select-sm");
    viewSelect.setAttribute("aria-label", "Value visualization");
    [
      ["tree", "Tree"],
      ["table", "Table"],
    ].forEach(function (view) {
      var option = element("option", null, view[1]);
      option.value = view[0];
      viewSelect.append(option);
    });
    var filter = element("input", "form-control form-control-sm");
    filter.type = "search";
    filter.placeholder = "Filter printed values or types";
    filter.setAttribute("aria-label", filter.placeholder);
    var typesLabel = element("label", "clojure-inspector-types-toggle");
    var types = element("input", "form-check-input");
    types.type = "checkbox";
    types.checked = previous
      ? previous.showTypes
      : compact
        ? false
        : payload["show-types"] !== false;
    typesLabel.append(types, document.createTextNode(" Types"));
    var copyPath = element(
      "button",
      "btn btn-sm btn-outline-secondary",
      "Copy get-in path",
    );
    copyPath.type = "button";
    copyPath.disabled = true;
    toolbar.append(rootSelect, viewSelect, filter, typesLabel, copyPath);
    var selection = element("div", "clojure-inspector-selection");
    var content = element("div", "clojure-inspector-content");
    var copyStatus = element("span", "visually-hidden");
    copyStatus.setAttribute("role", "status");
    copyStatus.setAttribute("aria-live", "polite");
    if (compact) container.append(content);
    else container.append(toolbar, selection, copyStatus, content);
    var filterByRoot = previous ? previous.filterByRoot : {};
    var viewByRoot = previous ? previous.viewByRoot : {};
    var activeRootId = rootSelect.value;
    filter.value = filterByRoot[activeRootId] || "";
    viewSelect.value = viewByRoot[activeRootId] || "tree";
    var state = {
      container: container,
      payload: payload,
      rootSelect: rootSelect,
      viewSelect: viewSelect,
      filter: filter,
      selection: selection,
      content: content,
      copyPath: copyPath,
      copyStatus: copyStatus,
      compact: compact,
      showTypes: types.checked,
      query: filter.value.trim(),
      activeRootId: activeRootId,
      filterByRoot: filterByRoot,
      viewByRoot: viewByRoot,
      expandedPaths: previous ? previous.expandedPaths : new Set(),
      collapsedPaths: previous ? previous.collapsedPaths : new Set(),
      initialDepth:
        options.initialDepth === undefined
          ? compact
            ? 3
            : 1
          : options.initialDepth,
      selectedNode: previous ? previous.selectedNode : null,
      selectedPath: previous ? previous.selectedPath : [],
      selectedPathId: previous ? previous.selectedPathId : null,
      selectedRootId: previous ? previous.selectedRootId : null,
      filterTimer: null,
    };
    var mountedRoot = currentRoot(state);
    if (mountedRoot && state.selectedRootId === mountedRoot.id)
      state.selectedNode = nodeAtSegments(mountedRoot.node, state.selectedPath);
    else state.selectedNode = null;
    container.__llarValueInspector = state;
    rootSelect.addEventListener("change", function () {
      state.filterByRoot[state.activeRootId] = state.filter.value;
      state.viewByRoot[state.activeRootId] = state.viewSelect.value;
      state.activeRootId = rootSelect.value;
      state.filter.value = state.filterByRoot[state.activeRootId] || "";
      state.query = state.filter.value.trim();
      state.viewSelect.value = state.viewByRoot[state.activeRootId] || "tree";
      state.selectedNode = null;
      render(state);
    });
    viewSelect.addEventListener("change", function () {
      render(state);
    });
    filter.addEventListener("input", function () {
      state.query = filter.value.trim();
      state.filterByRoot[state.activeRootId] = filter.value;
      global.clearTimeout(state.filterTimer);
      state.filterTimer = global.setTimeout(function () {
        render(state);
      }, 100);
    });
    types.addEventListener("change", function () {
      state.showTypes = types.checked;
      render(state);
    });
    copyPath.addEventListener("click", function () {
      copyText(pathToEdn(state.selectedPath)).then(
        function () {
          announceCopy(state, true, "Copied get-in path to the clipboard.");
        },
        function () {
          announceCopy(state, false, "Could not copy the get-in path.");
        },
      );
    });
    render(state);
    return state;
  }

  function fromJson(value) {
    if (value === null || value === undefined)
      return { kind: "scalar", "semantic-type": "nil", printed: "nil" };
    if (Array.isArray(value))
      return {
        kind: "collection",
        "semantic-type": "vector",
        "runtime-type": "json/array",
        open: "[",
        close: "]",
        count: value.length,
        "count-known": true,
        children: value.map(fromJson),
      };
    if (typeof value === "object")
      return {
        kind: "map",
        "semantic-type": "map",
        "runtime-type": "json/object",
        open: "{",
        close: "}",
        count: Object.keys(value).length,
        "count-known": true,
        children: Object.keys(value).map(function (key) {
          return {
            key: {
              kind: "scalar",
              "semantic-type": "string",
              "runtime-type": "json/string",
              printed: JSON.stringify(key),
            },
            value: fromJson(value[key]),
          };
        }),
      };
    var semantic = typeof value;
    return {
      kind: "scalar",
      "semantic-type": semantic,
      "runtime-type": "json/" + semantic,
      printed:
        typeof value === "string" ? JSON.stringify(value) : String(value),
    };
  }

  function mountAll(root) {
    root = root || document;
    root
      .querySelectorAll("[data-clojure-value-inspector]")
      .forEach(function (container) {
        if (container.__llarValueInspector) return;
        var closedDetails = container.closest("details:not([open])");
        if (closedDetails) {
          if (!container.dataset.clojureInspectorPending) {
            container.dataset.clojureInspectorPending = "true";
            var mountWhenOpened = function () {
              if (!closedDetails.open) return;
              closedDetails.removeEventListener("toggle", mountWhenOpened);
              delete container.dataset.clojureInspectorPending;
              mountAll(closedDetails);
            };
            closedDetails.addEventListener("toggle", mountWhenOpened);
          }
          return;
        }
        var payloadNode = container.querySelector(
          ".clojure-value-inspector-payload",
        );
        if (!payloadNode) return;
        try {
          mount(container, JSON.parse(payloadNode.textContent), {
            variant: container.dataset.clojureInspectorVariant,
          });
        } catch (error) {
          container.textContent =
            "Could not render value inspector: " + error.message;
          container.classList.add("alert", "alert-danger");
        }
      });
  }

  global.llarValueInspector = {
    fromJson: fromJson,
    mount: mount,
    mountAll: mountAll,
    nodeAtPath: nodeAtPath,
    nodeToEdn: nodeToEdn,
    pathToEdn: pathToEdn,
    tableSchema: tableSchema,
  };

  if (document.readyState === "loading")
    document.addEventListener("DOMContentLoaded", function () {
      mountAll(document);
    });
  else mountAll(document);
})(window);
