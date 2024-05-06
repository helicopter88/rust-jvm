#[cfg(test)]
mod enums_tests {
    use crate::rustvm::enums::{LocalVariable, ReferenceKind};
    use crate::rustvm::enums::ReferenceKind::Null;

    #[test]
    fn test_lower_to_common()
    {
        let mut item = LocalVariable::Double(1.0);
        assert_eq!(item.to_common_lv(), LocalVariable::Double(1.0));
        item = LocalVariable::Reference(ReferenceKind::ArrayReference(2));
        assert_eq!(item.to_common_lv(), LocalVariable::Long(2));
        item = LocalVariable::Int(12);
        assert_eq!(item.to_common_lv(), LocalVariable::Long(12));
    }
    
    #[test]
    fn test_lv_equality()
    {
        {
            let lhs = LocalVariable::Reference(ReferenceKind::ArrayReference(12));
            let rhs = LocalVariable::Reference(ReferenceKind::ArrayReference(12));
            assert_eq!(lhs, rhs);
        }
        {
            let lhs = LocalVariable::Reference(ReferenceKind::ArrayReference(12));
            let rhs = LocalVariable::Long(12);
            assert_ne!(lhs, rhs);
        }
        {
            let lhs = LocalVariable::Reference(ReferenceKind::ArrayReference(12));
            let rhs = LocalVariable::Long(55);
            assert_ne!(lhs, rhs);
        }
        {
            let lhs = LocalVariable::Double(12.);
            let rhs = LocalVariable::Long(55);
            assert_ne!(lhs, rhs);
        }
        {
            let lhs = LocalVariable::Address(12);
            let rhs = LocalVariable::Address(55);
            assert_ne!(lhs, rhs);
        }
        {
            let lhs = LocalVariable::Reference(Null());
            let rhs = LocalVariable::Reference(Null());
            assert_eq!(lhs, rhs);
        }
    }
}