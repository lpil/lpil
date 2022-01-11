fn main() {
    println!("Hello, world!");
}

// Attempting to use an Rc in C FFI

use std::rc::Rc;

#[repr(C)]
pub struct AudioClip {
    num_samples: std::os::raw::c_int,
    samples: *mut f32,
}

pub struct RcAudioClip(Rc<AudioClip>);

#[no_mangle]
pub extern "C" fn new(clip: AudioClip) -> *mut RcAudioClip {
    let obj = RcAudioClip(Rc::new(clip));
    // Copy it to the heap to get a stable pointer to it
    let boxed_obj = Box::new(obj);
    Box::into_raw(boxed_obj)
}

#[no_mangle]
pub unsafe extern "C" fn destroy(obj: *mut RcAudioClip) {
    if obj.is_null() {
        return;
    }

    let boxed = Box::from_raw(obj);
    drop(boxed);
}

