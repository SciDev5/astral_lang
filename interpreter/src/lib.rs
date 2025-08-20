use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use front::{
    AExpr, AExprEval, AExprLiteral, AExprPattern, AFunction, AModule, ANamespace, AType, AWhere,
    CoreRefs, FunctionId, GlobalId, ModuleId,
};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    CoreVoid,
    CoreBool(bool),
    CoreU32(u32),
}

struct CallFrame<'a> {
    vars: Vec<Value>,
    intermediates: Vec<Value>,
    instructions: &'a [IInstruction],
    next_instr: usize,
}
pub fn interpret(
    core: &CoreRefs,
    modules: &HashMap<ModuleId, Arc<AModule>>,
    entry: FunctionId,
) -> Value {
    let functions = translate_functions_in_modules(core, &modules);
    dbg!(&functions);

    let Some(IFunction::Instructions {
        n_args: 0,
        n_locals,
        instructions,
    }) = functions.get(&entry)
    else {
        panic!("entry point function must exist, not be a builtin, and have zero arguments");
    };

    let mut call_stack: Vec<CallFrame> = Vec::from([CallFrame {
        intermediates: Vec::new(),
        next_instr: 0,
        vars: std::iter::repeat_with(|| Value::CoreVoid)
            .take(*n_locals)
            .collect(),
        instructions,
    }]);

    loop {
        let current_frame = call_stack.last_mut().unwrap();
        let instr = if current_frame.next_instr == current_frame.instructions.len() {
            &IInstruction::Return
        } else {
            &current_frame.instructions[current_frame.next_instr]
        };
        dbg!(current_frame.next_instr, instr);
        let delta = match instr {
            IInstruction::ReadLocal(var_id) => {
                current_frame
                    .intermediates
                    .push(current_frame.vars[*var_id]);
                1
            }
            IInstruction::WriteLocal(var_id) => {
                current_frame.vars[*var_id] = current_frame.intermediates.pop().unwrap();
                current_frame.intermediates.push(Value::CoreVoid);
                1
            }
            IInstruction::Call(global_id) => {
                let f = functions.get(global_id).unwrap();
                let args = current_frame
                    .intermediates
                    .split_off(current_frame.intermediates.len() - f.n_args());
                match f {
                    IFunction::Builtin(f) => {
                        current_frame.intermediates.push(f.eval(args));
                    }
                    IFunction::Instructions {
                        n_locals,
                        instructions,
                        ..
                    } => {
                        current_frame.next_instr += 1;
                        let mut vars = args;
                        vars.extend(std::iter::repeat_with(|| Value::CoreVoid).take(*n_locals));
                        call_stack.push(CallFrame {
                            vars,
                            intermediates: Vec::new(),
                            instructions: &instructions,
                            next_instr: 0,
                        });
                        continue;
                    }
                }
                1
            }
            IInstruction::Return => {
                let return_value = current_frame.intermediates.pop().unwrap();
                call_stack.pop();
                if let Some(caller_frame) = call_stack.last_mut() {
                    caller_frame.intermediates.push(return_value);
                } else {
                    return return_value;
                }
                continue;
            }
            IInstruction::Jump(delta) => *delta,
            IInstruction::Branch(delta) => {
                let Some(Value::CoreBool(do_branch)) = current_frame.intermediates.pop() else {
                    panic!("if argument was not boolean");
                };
                if do_branch { *delta } else { 1 }
            }
            IInstruction::DiscardValue => {
                current_frame.intermediates.pop().unwrap();
                1
            }
            IInstruction::PushValue(value) => {
                current_frame.intermediates.push(**value);
                1
            }
        };
        current_frame.next_instr = current_frame.next_instr.checked_add_signed(delta).unwrap();
    }
}

fn translate_functions_in_modules(
    core: &CoreRefs,
    modules: &HashMap<ModuleId, Arc<AModule>>,
) -> HashMap<FunctionId, IFunction> {
    modules
        .iter()
        .filter(|(id, _)| **id != 0)
        .flat_map(|(module_id, module)| {
            module.functions.iter().enumerate().map(|(id, func)| {
                (
                    GlobalId {
                        module_id: *module_id,
                        id,
                    },
                    translate_function(
                        core,
                        func,
                        GlobalId {
                            module_id: *module_id,
                            id,
                        },
                    ),
                )
            })
        })
        .collect()
}

#[derive(Debug, Clone)]
enum IInstruction {
    ReadLocal(usize),
    WriteLocal(usize),
    Call(FunctionId),
    Return,
    Jump(isize),
    Branch(isize),
    DiscardValue,
    PushValue(Box<Value>),
}
#[derive(Debug)]
enum IFunction {
    Builtin(IFunctionBuiltin),
    Instructions {
        /// number of arguments
        n_args: usize,
        /// number of local variables (after arguments)
        n_locals: usize,
        instructions: Vec<IInstruction>,
    },
}
#[derive(Debug)]
enum IFunctionBuiltin {
    One,
    Print,
}
impl IFunctionBuiltin {
    fn n_args(&self) -> usize {
        match self {
            IFunctionBuiltin::One => 0,
            IFunctionBuiltin::Print => 1,
        }
    }
    fn eval(&self, args: Vec<Value>) -> Value {
        debug_assert_eq!(args.len(), self.n_args());
        match self {
            IFunctionBuiltin::One => Value::CoreU32(1),
            IFunctionBuiltin::Print => {
                println!(
                    "PRINTED VALUE: {}",
                    match args[0] {
                        Value::CoreU32(v) => v,
                        _ => panic!(),
                    }
                );
                Value::CoreVoid
            }
        }
    }
}
impl IFunction {
    fn n_args(&self) -> usize {
        match self {
            Self::Instructions { n_args, .. } => *n_args,
            Self::Builtin(builtin) => builtin.n_args(),
        }
    }
}

fn get_fn_core(core: &CoreRefs, func_id: FunctionId) -> IFunctionBuiltin {
    todo!()
}
fn get_fn_builtin(func_id: FunctionId) -> IFunctionBuiltin {
    match func_id.id {
        0 => IFunctionBuiltin::One,
        1 => IFunctionBuiltin::Print,
        _ => unreachable!(),
    }
}
fn translate_function(core: &CoreRefs, func: &AFunction, func_id: FunctionId) -> IFunction {
    if func_id.module_id == core.d_void.module_id {
        return IFunction::Builtin(get_fn_core(core, func_id));
    }
    if func_id.module_id == 1 {
        return IFunction::Builtin(get_fn_builtin(func_id));
    }
    let n_args = func.args_ty.len();
    let body = func.body.as_ref().unwrap();
    let n_locals = body.locals.len();
    let instructions = translate_expr(&body.expr);
    IFunction::Instructions {
        n_args,
        n_locals,
        instructions,
    }
}
fn translate_expr(expr: &AExpr) -> Vec<IInstruction> {
    match &expr.eval {
        AExprEval::Literal {
            value: AExprLiteral::Bool(value),
        } => Vec::from([IInstruction::PushValue(Box::new(Value::CoreBool(*value)))]),
        AExprEval::Literal {
            value: AExprLiteral::U32(value),
        } => Vec::from([IInstruction::PushValue(Box::new(Value::CoreU32(*value)))]),
        AExprEval::Literal { value } => todo!(),
        AExprEval::CallFunction {
            function_id,
            arguments,
        } => arguments
            .iter()
            .flat_map(translate_expr)
            .chain([IInstruction::Call(*function_id)])
            .collect(),
        AExprEval::CallMetatypeFunction {
            metatype_id,
            function_id,
            arguments,
        } => todo!(),
        AExprEval::Block { inner } => {
            let mut block_instructions = inner
                .iter()
                .flat_map(|expr| {
                    let mut expr_instructions = translate_expr(expr);
                    expr_instructions.push(IInstruction::DiscardValue); // discard value before next line
                    expr_instructions
                })
                .collect::<Vec<_>>();
            if block_instructions.is_empty() {
                let _ = block_instructions.push(IInstruction::PushValue(Box::new(Value::CoreVoid)));
            } else {
                let _ = block_instructions.pop(); // drop last pop so the value comes through
            }
            block_instructions
        }
        AExprEval::LocalRef { local_ref_id } => Vec::from([IInstruction::ReadLocal(*local_ref_id)]),
        AExprEval::Deref { reference } => todo!(),
        AExprEval::Assign {
            receiver: AExprPattern::VarBind { local_ref_id },
            value,
        } => {
            let mut expr_instructions = translate_expr(value);
            expr_instructions.push(IInstruction::WriteLocal(*local_ref_id));
            expr_instructions
        }
        AExprEval::Assign { receiver, value } => todo!(),
        AExprEval::DataInit { data_id, value } => todo!(),
        AExprEval::DataAccess { value, field } => todo!(),
        AExprEval::Return { value } => {
            let mut expr_instructions = translate_expr(value);
            expr_instructions.push(IInstruction::Return);
            expr_instructions
        }
    }
}

pub fn interpreter_builtins(core: CoreRefs) -> Arc<AModule> {
    Arc::new(AModule {
        global_id: 1,
        deps: HashSet::new(),
        wheres: Vec::from([AWhere {
            n_vars: 0,
            parent_id: None,
            constraints: Vec::new(),
        }]),
        datas: Vec::from([]),
        functions: Vec::from([
            AFunction {
                // one
                body: None,
                where_id: 0,
                args_ty: Vec::from([]),
                return_ty: AType::Data {
                    data_id: core.d_u32,
                    bindings: Vec::new(),
                },
            },
            AFunction {
                // print_u32
                body: None,
                where_id: 0,
                args_ty: Vec::from([AType::Data {
                    data_id: core.d_u32,
                    bindings: Vec::new(),
                }]),
                return_ty: AType::Data {
                    data_id: core.d_void,
                    bindings: Vec::new(),
                },
            },
        ]),
        metatypes: Vec::from([]),
        metatype_impls: Vec::from([]),
        namespaces: Vec::from([ANamespace {
            // todo: export
        }]),
    })
}
