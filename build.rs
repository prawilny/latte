use cfgrammar::yacc::YaccKind;
use lrlex::LexerBuilder;
use lrpar::{CTParserBuilder};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let lex_rule_ids_map = CTParserBuilder::new()
        .yacckind(YaccKind::Grmtools)
        .recoverer(lrpar::RecoveryKind::None)
        .process_file_in_src("latte.y")?;
    LexerBuilder::new()
        .rule_ids_map(lex_rule_ids_map)
        .process_file_in_src("latte.l")?;
    Ok(())
}