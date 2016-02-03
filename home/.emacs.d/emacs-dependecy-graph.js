'use strict'
var _ = require('lodash')
var graphlib = require('graphlib')
var Graph = graphlib.Graph
var graphAlg = graphlib.alg

/*
 * A refract element implementation with an extensible namespace, able to
 * load other namespaces into it.
 *
 * The namespace allows you to register your own classes to be instantiated
 * when a particular refract element is encountered, and allows you to specify
 * which elements get instantiated for existing Javascript objects.
 */
function Namespace(options) {
  options || (options = {})
  return this
}

/* Graph Notes
 *
Graph = require('graphlib').Graph
graph = new Graph({directed: true})
graph.setEdge('User', 'object')
graph.setEdge('Customer', 'User')
graph.setEdge('SuperCustomer', 'Customer')
graph.setEdge('Fish', 'object')
graph.setEdge('Person', 'object')

// v: the id of the source or tail node of an edge
// w: the id of the target or head node of an edge

graph.sources() // [ 'SuperCustomer', 'Fish', 'Person' ]
graph.sinks() // [ 'object' ]
graph.predecessors('object') // [ 'User', 'Fish', 'Person' ]
graph.predecessors('Customer') // [ 'SuperCustomer' ]
graph.predecessors('User') // [ 'Customer' ]
graph.outEdges('Fish') // [ { v: 'Fish', w: 'object' } ]
graph.outEdges('SuperCustomer') // [ { v: 'SuperCustomer', w: 'Customer' } ]
graph.nodes() // [ 'User', 'object', 'Customer', 'SuperCustomer', 'Fish', 'Person' ]
graph.nodeEdges('User')
// [ { v: 'Customer', w: 'User' },
//   { v: 'User', w: 'object' } ]
graph.nodeEdges('Customer')
// [ { v: 'SuperCustomer', w: 'Customer' },
//   { v: 'Customer', w: 'User' } ]
graph.nodeEdges('SuperCustomer')
// [ { v: 'SuperCustomer', w: 'Customer' } ]
graph.nodeEdges('object')
// [ { v: 'User', w: 'object' },
//   { v: 'Fish', w: 'object' },
//   { v: 'Person', w: 'object' } ]
graph.neighbors('object') // [ 'User', 'Fish', 'Person' ]
graph.neighbors('Fish') // [ 'object' ]
graph.neighbors('User') // [ 'Customer', 'object' ]
graph.neighbors('Customer') // [ 'SuperCustomer', 'User' ]
graph.edges()
// [ { v: 'User', w: 'object' },
//   { v: 'Customer', w: 'User' },
//   { v: 'SuperCustomer', w: 'Customer' },
//   { v: 'Fish', w: 'object' },
//   { v: 'Person', w: 'object' } ]
graph.successors('object') // []
graph.successors('SuperCustomer') // [ 'Customer' ]
graph.successors('Customer') // [ 'User' ]
graph.successors('User') // [ 'object' ]
*/

Namespace.prototype.setEdge = function (parent, child) {
  var that=this
  if (_.isArray(parent) && _.isArray(child)) {
    _.forEach(parent, function(p) {
      _.forEach(child, function(c) {
        that.graph.setEdge(p, c)
      })
    })
  }
  else if (_.isArray(parent)) {
    _.forEach(parent, function(p) {
      that.graph.setEdge(p, child)
    })
  }
  else if (_.isArray(child)) {
    _.forEach(child, function(c) {
      that.graph.setEdge(parent, c)
    })
  }
  else {
    this.graph.setEdge(parent, child)
  }
}

Namespace.prototype.successor = function (node) {
  var successors = this.graph.successors(node)
  if (successors.length > 1)
    throw new VError('successor fail: %s only supposed to have single inheritance', node)

  return _.head(successors)
}

Namespace.prototype.successors = function (node) {
  return this.graph.successors(node)
}

Namespace.prototype.predecessors = function (node) {
  return this.graph.predecessors(node)
}

Namespace.prototype.topsort = function (node) {
  return graphAlg.topsort(this.graph)
}

Namespace.prototype.successorChain = function (node) {
  var sinks = this.graph.sinks() // [ 'object' ]

  if (_.isEmpty(sinks))
    throw new VError('successorChain fail: graph is empty', node)

  var sink = _.head(sinks)
  if (sink !== 'object')
    throw new VError('successorChain fail: %s expected `object` to be root', node)

  var successor = this.successor(node)
  if (!successor)
    return

  var chain = []
  while (successor !== sink) {
    chain.push(successor)
    successor = this.successor(successor)
  }
  chain.push(successor)
  return chain
}

Object.defineProperty(Namespace.prototype, 'graph', {
  get: function () {
    if (!this._graph)
      this._graph = new Graph({
        directed: true,
      })

    return this._graph
  },
  set: function (element) {
    this._baseElement = element
  },
})

// module.exports = ns
function main() {
  var ns = new Namespace()

  // module-evil.el
  // with-eval-after-loads
  // ns.setEdge('evil', ['dired', 'smartparens', 'evil-surround'])

  // module-shell.el
  // with-eval-after-loads
  // ns.setEdge('shell', 'esh-mode')
  ns.setEdge('shell', ['smooth-scrolling', 'magit'])

  // module-eshell
  ns.setEdge('eshell', ['company'])

  // c-c++
  ns.setEdge('cmake-mode', 'company')
  ns.setEdge(['c-mode', 'c++-mode'], 'flycheck')
  ns.setEdge(['c-mode', 'c++-mode'], 'helm-gtags')
  ns.setEdge(['c-mode', 'c++-mode'], 'semantic')
  ns.setEdge(['c-mode', 'c++-mode'], 'stickyfunc-enhance')
  ns.setEdge('c++-mode', 'ycmd')
  ns.setEdge('c-mode', 'company-ycmd')

  // clojure
  ns.setEdge(['cider-mode', 'cider-repl-mode'], 'company')

  // elisp
  ns.setEdge('flycheck-package', 'flycheck')
  ns.setEdge(['emacs-lisp-mode', 'ielm-mode'], 'company')
  ns.setEdge(['emacs-lisp-mode', 'lisp-mode'], 'flycheck')
  ns.setEdge(['emacs-lisp-mode'], 'semantic')
  ns.setEdge(['emacs-lisp-mode'], 'srefactor')

  // elm
  ns.setEdge('elm-mode', 'flycheck')
  ns.setEdge('elm-mode', 'smartparens')

  // github
  ns.setEdge('github-mode', 'magit')

  console.log(ns.topsort())

}

main()
