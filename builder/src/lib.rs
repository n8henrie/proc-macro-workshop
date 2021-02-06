use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _input = parse_macro_input!(input as DeriveInput);

    let expanded = quote! {
        impl Command {
            pub fn builder() -> CommandBuilder {
                CommandBuilder {
                    executable: None,
                    args: None,
                    env: None,
                    current_dir: None,
                }
            }
        }

     pub struct CommandBuilder {
         executable: Option<String>,
         args: Option<Vec<String>>,
         env: Option<Vec<String>>,
         current_dir: Option<String>,
     }

     impl CommandBuilder {
         fn executable(&mut self, executable: String) -> &mut Self {
             self.executable = Some(executable);
             self
         }
         fn args(&mut self, args: Vec<String>) -> &mut Self {
             self.args = Some(args);
             self
         }
         fn env(&mut self, env: Vec<String>) -> &mut Self {
             self.env = Some(env);
             self
         }
         fn current_dir(&mut self, current_dir: String) -> &mut Self {
             self.current_dir = Some(current_dir);
             self
        }
        pub fn build(&mut self) -> Result<Command, Box<dyn std::error::Error>> {
            Ok(Command{
                executable: self.executable.take().ok_or("executable is unset")?,
                args: self.args.take().ok_or("args is unset")?,
                env: self.env.take().ok_or("env is unset")?,
                current_dir: self.current_dir.take().ok_or("current_dir is unset")?,
            })
        }
     }
    };
    TokenStream::from(expanded)
}
