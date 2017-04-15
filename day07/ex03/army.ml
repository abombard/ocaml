class ['a] army =
    object (self)
        val _instances : 'a list = []

        method getInstances = _instances

        method add p = {< _instances = p :: _instances >}
        method delete = {< _instances = List.tl self#getInstances >}

        method size = List.length self#getInstances
    end
