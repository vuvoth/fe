object \"Contract\" {
     code {
         let size := datasize(\"runtime\") datacopy(0, dataoffset(\"runtime\"), size) return(0, size) 
    }
    object \"runtime\" {
         code {
             function $$bar() -> return_val {
                 let $building := struct_House_new(300, 500) if iszero(eq(mloadn(struct_House_get_size($building), 32), 500)) {
                     revert(0, 0) 
                }
                if iszero(eq(mloadn(struct_House_get_price($building), 32), 300)) {
                     revert(0, 0) 
                }
                {
                     return_val := mloadn(struct_House_get_size($building), 32) leave 
                }
                
            }
            function avail() -> ptr {
                 ptr := mload(0x00) if eq(ptr, 0x00) {
                     ptr := 0x20 
                }
                
            }
            function alloc(size) -> ptr {
                 ptr := mload(0x00) if eq(ptr, 0x00) {
                     ptr := 0x20 
                }
                mstore(0x00, add(ptr, size)) 
            }
            function alloc_mstoren(val, size) -> ptr {
                 ptr := alloc(size) mstoren(ptr, val, size) 
            }
            function free(ptr) {
                 mstore(0x00, ptr) 
            }
            function ccopym(cptr, size) -> mptr {
                 mptr := alloc(size) calldatacopy(mptr, cptr, size) 
            }
            function load_data_string(code_ptr, size) -> mptr {
                 mptr := alloc(32) mstore(mptr, size) let content_ptr := alloc(size) datacopy(content_ptr, code_ptr, size) 
            }
            function encode_dummy_tuple(val_1, val_2) -> mptr {
                 mptr := alloc(32) mstore(mptr, val_1) let second_ptr := alloc(32) mstore(second_ptr, val_2) 
            }
            function mcopys(mptr, sptr, size) {
                 let offset := 0 for {
                     
                }
                lt(add(offset, 32), size) {
                     
                }
                {
                     let _mptr := add(mptr, offset) let _sptr := add(sptr, offset) sstore(_sptr, mload(_mptr)) offset := add(offset, 32) 
                }
                let rem := sub(size, offset) if gt(rem, 0) {
                     let _mptr := add(mptr, offset) let _sptr := add(sptr, offset) sstoren(_sptr, mloadn(_mptr, rem), rem) 
                }
                
            }
            function scopym(sptr, size) -> mptr {
                 mptr := alloc(size) let offset := 0 for {
                     
                }
                lt(add(offset, 32), size) {
                     
                }
                {
                     let _mptr := add(mptr, offset) let _sptr := add(sptr, offset) mstore(_mptr, sload(_sptr)) offset := add(offset, 32) 
                }
                let rem := sub(size, offset) if gt(rem, 0) {
                     let _mptr := add(mptr, offset) let _sptr := add(sptr, offset) mstoren(_mptr, sloadn(_sptr, rem), rem) 
                }
                
            }
            function mcopym(ptr1, size) -> ptr2 {
                 ptr2 := alloc(size) let offset := 0 for {
                     
                }
                lt(add(offset, 32), size) {
                     
                }
                {
                     let _ptr1 := add(ptr1, offset) let _ptr2 := add(ptr2, offset) mstore(_ptr2, mload(_ptr1)) offset := add(offset, 32) 
                }
                let rem := sub(size, offset) if gt(rem, 0) {
                     let _ptr1 := add(ptr1, offset) let _ptr2 := add(ptr2, offset) mstoren(_ptr2, mloadn(_ptr1, rem), rem) 
                }
                
            }
            function scopys(ptr1, ptr2, size) {
                 let offset := 0 for {
                     
                }
                lt(add(offset, 32), size) {
                     
                }
                {
                     let _ptr1 := add(ptr1, offset) let _ptr2 := add(ptr2, offset) sstore(_ptr2, sload(_ptr1)) offset := add(offset, 32) 
                }
                let rem := sub(size, offset) if gt(rem, 0) {
                     let _ptr1 := add(ptr1, offset) let _ptr2 := add(ptr2, offset) sstoren(_ptr2, sloadn(_ptr1, rem), rem) 
                }
                
            }
            function mloadn(ptr, size) -> val {
                 val := shr(sub(256, mul(8, size)), mload(ptr)) 
            }
            function sloadn(ptr, size) -> val {
                 val := shr(sub(256, mul(8, size)), sload(ptr)) 
            }
            function cloadn(ptr, size) -> val {
                 val := shr(sub(256, mul(8, size)), calldataload(ptr)) 
            }
            function mstoren(ptr, val, size) {
                 let size_bits := mul(8, size) let left := shl(sub(256, size_bits), val) let right := shr(size_bits, mload(add(ptr, size))) mstore(ptr, or(left, right)) 
            }
            function sstoren(ptr, val, size) {
                 let size_bits := mul(8, size) let left := shl(sub(256, size_bits), val) let right := shr(size_bits, sload(add(ptr, size))) sstore(ptr, or(left, right)) 
            }
            function dualkeccak256(a, b) -> return_val {
                 let ptr := avail() mstore(ptr, a) mstore(add(ptr, 32), b) return_val := keccak256(ptr, 64) 
            }
            function ceil32(n) -> return_val {
                 return_val := mul(div(add(n, 31), 32), 32) 
            }
            function ternary(test, if_expr, else_expr) -> result {
                 switch test case 1 {
                     result := if_expr 
                }
                case 0 {
                     result := else_expr 
                }
                 
            }
            function abi_unpack(mptr, array_size, inner_data_size) {
                 for {
                     let i := 0 
                }
                lt(i, array_size) {
                     i := add(i, 1) 
                }
                {
                     let val_ptr := add(mptr, mul(i, inner_data_size)) let val := mloadn(val_ptr, inner_data_size) pop(alloc_mstoren(val, 32)) 
                }
                
            }
            function abi_pack_calldata(mptr, array_size, inner_data_size) -> packed_ptr {
                 packed_ptr := avail() for {
                     let i := 0 
                }
                lt(i, array_size) {
                     i := add(i, 1) 
                }
                {
                     let val_ptr := add(mptr, mul(i, 32)) let val := calldataload(val_ptr) pop(alloc_mstoren(val, inner_data_size)) 
                }
                
            }
            function abi_pack_mem(mptr, array_size, inner_data_size) -> packed_ptr {
                 packed_ptr := avail() for {
                     let i := 0 
                }
                lt(i, array_size) {
                     i := add(i, 1) 
                }
                {
                     let val_ptr := add(mptr, mul(i, 32)) let val := mload(val_ptr) pop(alloc_mstoren(val, inner_data_size)) 
                }
                
            }
            function abi_encode_uint256(val_0) -> ptr {
                 ptr := avail() pop(alloc_mstoren(val_0, 32)) 
            }
            function struct_House_new(price, size) -> return_val {
                 return_val := alloc(32) mstore(return_val, price) let size_ptr := alloc(32) mstore(size_ptr, size) 
            }
            function struct_House_get_price(ptr) -> return_val {
                 return_val := add(ptr, 0) 
            }
            function struct_House_get_size(ptr) -> return_val {
                 return_val := add(ptr, 32) 
            }
            switch cloadn(0, 4) case 0xfebb0f7e {
                 let raw_return := $$bar() return(abi_encode_uint256(raw_return), add(32, 0)) 
            }
             
        }
          
    }
     
}
