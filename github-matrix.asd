(defsystem "github-matrix"
  :version "0.3.1"
  :author "Alexander Artemenko"
  :license "Unlicense"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("github-matrix/playground")
  :description "A server to render SVG representation of Github Action's results."
  :in-order-to ((test-op (test-op github-matrix-test))))

