#[cfg(test)]
mod executors_test {
    use std::collections::VecDeque;

    use crate::rustvm::enums::LocalVariable;
    use crate::rustvm::executors::{execute_add, execute_iload_n, execute_load_integer_const, execute_sub, Stack};

    #[test]
    fn load_constants() {
        let mut stack = VecDeque::<LocalVariable>::new();
        execute_load_integer_const(0x2, &mut stack);
        assert!(!stack.is_empty());
        assert_eq!(1, stack.len());
        assert_eq!(LocalVariable::Int(-1), stack.pop_front().unwrap());
        execute_load_integer_const(0x2, &mut stack);
        execute_load_integer_const(0x3, &mut stack);
        execute_load_integer_const(0x4, &mut stack);
        execute_load_integer_const(0x5, &mut stack);
        execute_load_integer_const(0x6, &mut stack);
        assert!(!stack.is_empty());
        assert_eq!(5, stack.len());
        for number in &[3, 2, 1, 0]
        {
            assert_eq!(LocalVariable::Int(*number), stack.pop_front().unwrap());
        }
    }

    #[test]
    fn load_locals() {
        let mut stack = VecDeque::<LocalVariable>::new();
        let locals = vec![LocalVariable::Int(3)];
        {
            assert!(execute_iload_n(0x1A, &locals, &mut stack).is_ok());
            assert!(!stack.is_empty());
            assert_eq!(1, stack.len());
            assert_eq!(LocalVariable::Int(3), stack.pop_front().unwrap());
        }
        {
            assert!(execute_iload_n(0x1B, &locals, &mut stack).is_err());
            assert!(stack.is_empty());
            assert_eq!(0, stack.len());
        }
        {
            // Boolean gets converted into an integer due to docs
            let locals = vec![LocalVariable::Int(3), LocalVariable::Boolean(true)];
            assert!(execute_iload_n(0x1B, &locals, &mut stack).is_ok());
            assert!(!stack.is_empty());
            assert_eq!(1, stack.len());
            assert_eq!(LocalVariable::Int(1), stack.pop_front().unwrap());
        }
    }

    #[test]
    fn test_sum() {
        {
            let mut stack = Stack::from(vec![LocalVariable::Int(2), LocalVariable::Int(2)]);
            assert!(execute_add(&mut stack).is_ok());
            assert!(!stack.is_empty());
            assert_eq!(1, stack.len());
            assert_eq!(LocalVariable::Int(4), stack.pop_front().unwrap());
        }
        {
            let mut stack = Stack::from(vec![LocalVariable::Long(2 << 32), LocalVariable::Long(3 << 32)]);
            assert!(execute_add(&mut stack).is_ok());
            assert!(!stack.is_empty());
            assert_eq!(1, stack.len());
            assert_eq!(LocalVariable::Long(5 << 32), stack.pop_front().unwrap());
        }
        {
            let mut stack = Stack::from(vec![LocalVariable::Long(2 << 32), LocalVariable::Int(3)]);
            assert!(execute_add(&mut stack).is_err());
            assert!(stack.is_empty());
        }
    }

    #[test]
    fn test_sub() {
        {
            let mut stack = Stack::from(vec![LocalVariable::Int(2), LocalVariable::Int(2)]);
            assert!(execute_sub(&mut stack).is_ok());
            assert!(!stack.is_empty());
            assert_eq!(1, stack.len());
            assert_eq!(LocalVariable::Int(0), stack.pop_front().unwrap());
        }
        {
            let mut stack = Stack::from(vec![LocalVariable::Long(2 << 32), LocalVariable::Long(3 << 32)]);
            assert!(execute_sub(&mut stack).is_ok());
            assert!(!stack.is_empty());
            assert_eq!(1, stack.len());
            assert_eq!(LocalVariable::Long(1 << 32), stack.pop_front().unwrap());
        }
        {
            let mut stack = Stack::from(vec![LocalVariable::Long(2 << 32), LocalVariable::Int(3)]);
            assert!(execute_sub(&mut stack).is_err());
            assert!(stack.is_empty());
        }
    }
}
