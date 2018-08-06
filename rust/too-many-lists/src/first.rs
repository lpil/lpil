use std::mem;

pub struct List {
    head: Link,
}

enum Link {
    Empty,
    More(Box<Node>),
}

struct Node {
    elem: i32,
    next: Link,
}

impl List {

    pub fn new() -> Self {
        List { head: Link::Empty }
    }

    // The `push` method mutates the list, so it takes a mutable reference to itself.
    pub fn push(&mut self, elem: i32) {

        // Create a new node to be the new head of the list
        //
        let new_node = Box::new(Node {
            elem: elem,

            // We temporarily replace self.head with Link::Empty before replacing it with the new
            // head of the list.
            // This isn't great, but it'll work for now.
            //
            next: mem::replace(&mut self.head, Link::Empty),
        });
        self.head = Link::More(new_node);
    }


    // The `pop` function mutates the list, so it takes a mutable reference to itself.
    // It optionally returns a i32. It does not when the list is empty.
    //
    pub fn pop(&mut self) -> Option<i32> {
        match mem::replace(&mut self.head, Link::Empty) {
            Link::Empty =>
                None,
            Link::More(boxed_node) => {
                // Pull the node out of the box, copying it.
                // This is possible as all basic numeric types are Copy.
                let node = *boxed_node;
                self.head = node.next;
                Some(node.elem)
            }
        }
    }
}

impl Drop for List {
    fn drop(&mut self) {
        let mut current_link = mem::replace(&mut self.head, Link::Empty);
        // while let means do until the pattern does not match
        while let Link::More(mut boxed_node) = current_link {
            current_link = mem::replace(&mut boxed_node.next, Link::Empty);
            // boxed_node goes out of scope and gets dropped here;
            // but its Node's `next` field has been set to Link::Empty
            // so no unbounded recursion occurs.
        }
    }
}


#[cfg(test)]
mod test {
    use super::List;

    #[test]
    fn pop_empty_list() {
        // An empty list returns Option::None as it has nothing to return.
        let mut list = List::new();
        assert_eq!(list.pop(), None);
    }

    #[test]
    fn pop_push_basics() {
        let mut list = List::new();
        list.push(1);
        list.push(2);
        list.push(3);
        assert_eq!(list.pop(), Some(3));
        assert_eq!(list.pop(), Some(2));
        list.push(4);
        list.push(5);
        assert_eq!(list.pop(), Some(5));
        assert_eq!(list.pop(), Some(4));
        assert_eq!(list.pop(), Some(1));
        assert_eq!(list.pop(), None);
    }
}
