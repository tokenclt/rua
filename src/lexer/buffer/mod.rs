use std::iter;
use std::default::Default;

const BUFFER_SIZE: usize = 5;

#[derive(Debug, Clone)]
pub struct Buffer<Tit>
    where Tit: iter::Iterator<Item = char> + Clone
{
    current: usize,
    loaded: usize,
    marked: usize,
    wrapped_iter: Tit,

    is_marked: bool,
    storage: [Option<char>; BUFFER_SIZE],
}

impl<Tit> iter::Iterator for Buffer<Tit>
    where Tit: iter::Iterator<Item = char> + Clone
{
    type Item = char;

    fn next(&mut self) -> Option<char> {
        self.try_load_one();
        self.current = Self::increment(self.current);
        self.storage[self.current]
    }
}

impl<Tit> Buffer<Tit>
    where Tit: iter::Iterator<Item = char> + Clone
{
    pub fn new(it: Tit) -> Buffer<Tit> {
        Buffer {
            current: 0,
            loaded: 0,
            marked: 0,
            wrapped_iter: it,
            is_marked: false,
            storage: [Default::default(); BUFFER_SIZE],
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        self.try_load_one();
        self.storage[Self::increment(self.current)]
    }

    pub fn mark(&mut self) {
        self.is_marked = true;
        self.marked = self.current;
    }

    pub fn rewind(&mut self) {
        if !self.is_marked {
            panic!("Must mark before rewinding");
        }
        self.current = self.marked;
        self.is_marked = false;
    }

    fn try_load_one(&mut self) {
        if self.current == self.loaded {
            self.loaded = Self::increment(self.loaded);
            if self.is_marked && self.loaded == self.marked {
                panic!("Buffer is full.");
            }
            self.storage[self.loaded] = self.wrapped_iter.next();
        }
    }

    fn increment(index: usize) -> usize {
        (index + 1) % BUFFER_SIZE
    }
}

mod tests {
    use super::*;

    #[test]
    fn simple_test() {
        let text = "abc".to_string();
        let mut buffer = Buffer::new(text.chars());
        assert_eq!(buffer.peek(), Some('a'));
        assert_eq!(buffer.next(), Some('a'));
        buffer.mark();
        assert_eq!(buffer.next(), Some('b'));
        assert_eq!(buffer.peek(), Some('c'));
        assert_eq!(buffer.next(), Some('c'));
        assert_eq!(buffer.next(), None);
        buffer.rewind();
        assert_eq!(buffer.peek(), Some('b'));
        assert_eq!(buffer.next(), Some('b'));
        assert_eq!(buffer.next(), Some('c'));
        assert_eq!(buffer.next(), None);
        assert_eq!(buffer.next(), None);
    }

    #[test]
    #[should_panic]
    fn unpaired_rewind() {
        let text = "abc".to_string();
        let mut buffer = Buffer::new(text.chars());
        assert_eq!(buffer.next(), Some('a'));
        buffer.rewind();
    }

    #[test]
    fn longer_text() {
        let text = "abcdef".to_string();
        let mut buffer = Buffer::new(text.chars());
        assert_eq!(buffer.next(), Some('a'));
        assert_eq!(buffer.next(), Some('b'));
        assert_eq!(buffer.next(), Some('c'));
        assert_eq!(buffer.next(), Some('d'));
        assert_eq!(buffer.next(), Some('e'));
        assert_eq!(buffer.next(), Some('f'));
        assert_eq!(buffer.next(), None);
    }
}