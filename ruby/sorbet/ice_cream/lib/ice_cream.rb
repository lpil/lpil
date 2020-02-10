# typed: strict
require "ice_cream/version"

module IceCream
  class Error < StandardError; end

  class Box
    extend T::Sig
    extend T::Generic
    Elem = type_member

    sig { params(elem: Elem).void }
    def initialize(elem)
      @elem = elem
    end

    sig do
      type_parameters(:new_elem)
        .params(
          block: T.proc.params(elem: Elem).returns(T.type_parameter(:new_elem)),
        )
        .returns(Box[T.type_parameter(:new_elem)])
    end
    def map(&block)
      Box.new(block.call(@elem))
    end
  end
end
