[
  {
    "cmd": "insert-context",
    "context": "author",
    "schema": {
      "regexp": "\\w*",
      "type": "text"
    }
  },
  {
    "cmd": "insert-context",
    "context": "category",
    "schema": {
      "default": false,
      "regexp": "\\w*",
      "type": "text"
    }
  },
  {
    "cmd": "insert-context",
    "context": "dependencies",
    "schema": {
      "default": false,
      "regexp": "[^ ]*",
      "type": "text"
    }
  },
  {
    "cmd": "insert-context",
    "context": "description",
    "schema": {
      "regexp": "\\w*",
      "type": "text",
      "weight": 0.3
    }
  },
  {
    "cmd": "insert-context",
    "context": "hierarchy",
    "schema": {
      "regexp": "\\w*",
      "type": "text",
      "weight": 0.1
    }
  },
  {
    "cmd": "insert-context",
    "context": "indexed",
    "schema": {
      "default": false,
      "regexp": "[0-9]{4}(-[0-9]{2}(-[0-9]{2}(T[0-9]{2}:[0-9]{2}:[0-9]{2})?)?)?",
      "type": "date"
    }
  },
  {
    "cmd": "insert-context",
    "context": "maintainer",
    "schema": {
      "default": false,
      "regexp": "\\w*",
      "type": "text"
    }
  },
  {
    "cmd": "insert-context",
    "context": "module",
    "schema": {
      "regexp": ".*",
      "type": "text",
      "weight": 0.5
    }
  },
  {
    "cmd": "insert-context",
    "context": "name",
    "schema": {
      "regexp": "[^ ]*",
      "type": "text",
      "weight": 3
    }
  },
  {
    "cmd": "insert-context",
    "context": "package",
    "schema": {
      "regexp": ".*",
      "type": "text"
    }
  },
  {
    "cmd": "insert-context",
    "context": "partial",
    "schema": {
      "regexp": "[^ ]*",
      "type": "text",
      "weight": 0.2
    }
  },
  {
    "cmd": "insert-context",
    "context": "source",
    "schema": {
      "default": false,
      "regexp": ".*",
      "type": "text",
      "weight": 0.1
    }
  },
  {
    "cmd": "insert-context",
    "context": "synopsis",
    "schema": {
      "regexp": "\\w*",
      "type": "text",
      "weight": 0.8
    }
  },
  {
    "cmd": "insert-context",
    "context": "type",
    "schema": {
      "default": false,
      "regexp": "\\w*",
      "type": "text",
      "weight": 0.0
    }
  },
  {
    "cmd": "insert-context",
    "context": "upload",
    "schema": {
      "default": false,
      "regexp": "[0-9]{4}(-[0-9]{2}(-[0-9]{2}(T[0-9]{2}:[0-9]{2}:[0-9]{2})?)?)?",
      "type": "date"
    }
  },
  {
    "cmd": "insert-context",
    "context": "version",
    "schema": {
      "default": false,
      "regexp": ".*",
      "type": "text"
    }
  },
  {
    "cmd": "insert-context",
    "context": "signature",
    "schema": {
      "default": false,
      "regexp": "[^$\n]*",
      "type": "text"
    }
  },
  {
    "cmd": "insert-context",
    "context": "subsig",
    "schema": {
      "default": false,
      "regexp": "[^$\n]*",
      "type": "text",
      "weight": 0.5
    }
  }
]