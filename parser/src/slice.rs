use std::str::{
    from_utf8,
    Utf8Error,
};

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Slice<T> {
    ptr: *const T,
    len: usize,
}

impl<T> Slice<T> {
    pub fn new(slice: &[T]) -> Self {
        Self {
            ptr: slice.as_ptr(),
            len: slice.len(),
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    #[inline]
    unsafe fn bump(&self) -> Self {
        Self {
            ptr: self.ptr.offset(1),
            len: self.len - 1,
        }
    }

    #[inline]
    fn item(&self) -> Option<&T> {
        if self.is_empty() {
            None
        } else {
            Some(unsafe { &*self.ptr })
        }
    }

    #[inline]
    pub fn next(&self) -> Option<(Self, &T)> {
        self.item().map(|i| (unsafe { self.bump() }, i))
    }

    #[inline]
    pub unsafe fn as_slice(&self) -> &[T] {
        std::slice::from_raw_parts(self.ptr, self.len)
    }
}

impl Slice<u8> {
    pub fn from_str(string: &str) -> Self {
        let slice = string.as_bytes();
        Self {
            ptr: slice.as_ptr(),
            len: slice.len(),
        }
    }

    #[inline]
    pub unsafe fn as_str(&self) -> Result<&str, Utf8Error> {
        from_utf8(self.as_slice())
    }
}

pub mod serde_str {
    use std::fmt;

    use serde::de::{
        self,
        Visitor,
    };
    use serde::{
        Deserializer,
        Serializer,
    };

    use super::Slice;

    pub fn serialize<S>(value: &Slice<u8>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(unsafe { value.as_str().unwrap() })
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Slice<u8>, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(StrVisitor)
    }

    struct StrVisitor;

    impl<'de> Visitor<'de> for StrVisitor {
        type Value = Slice<u8>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a string")
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Slice::from_str(value))
        }
    }
}
