mod event;

use serde::{Deserialize, Serialize};
use std::io;
use termion::event::Key;
use termion::input::MouseTerminal;
use termion::raw::IntoRawMode;
use termion::screen::AlternateScreen;
use tui::backend::TermionBackend;
use tui::layout::{Alignment, Constraint, Direction, Layout};
use tui::style::{Color, Modifier, Style};
use tui::widgets::{Block, Borders, Paragraph, SelectableList, Text, Widget};
use tui::Terminal;

use crate::event::{Event, Events};

struct App {
    surveys: Vec<Survey>,
    selected: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Survey {
    id: i32,
    title: String,
    description: Option<String>,
    colour: Option<String>,
    tags: Vec<String>,
    date: chrono::DateTime<chrono::Utc>,
}

fn main() -> Result<(), failure::Error> {
    #[derive(Debug, Serialize, Deserialize)]
    struct RespBody {
        surveys: Vec<Survey>,
    }

    println!("Connecting to HappyLabs...");
    let RespBody { surveys } = reqwest::Client::new()
        .get("https://b7f836b1-6cad-4f88-94d2-0ef92541f5f3.mock.pstmn.io/surveys")
        // .get("http://api.happylabs.io/surveys")
        .send()?
        .json()?;

    let stdout = io::stdout().into_raw_mode()?;
    let stdout = MouseTerminal::from(stdout);
    let stdout = AlternateScreen::from(stdout);
    let backend = TermionBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    terminal.hide_cursor()?;

    let events = Events::new();

    // App
    let mut app = App {
        selected: 0,
        surveys,
    };

    loop {
        terminal.draw(|mut f| {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints([Constraint::Length(10), Constraint::Percentage(100)].as_ref())
                .split(f.size());

            let block = Block::default()
                .borders(Borders::ALL)
                .title_style(Style::default().modifier(Modifier::BOLD));

            let style = Style::default();
            SelectableList::default()
                .block(block.clone().title("✨ HappyLabs Surveys ✨"))
                .items(
                    &app.surveys
                        .iter()
                        .map(|s| &s.title)
                        .collect::<Vec<&String>>(),
                )
                .select(Some(app.selected))
                .style(style)
                .highlight_style(style.fg(Color::Cyan).modifier(Modifier::BOLD))
                .highlight_symbol("😁")
                .render(&mut f, chunks[0]);

            let Survey {
                description,
                title,
                tags,
                ..
            } = app.surveys[app.selected].clone();

            let text = [
                Text::styled(title, Style::default().modifier(Modifier::BOLD)),
                Text::raw("\n\n"),
                Text::raw(description.unwrap_or_else(|| String::new())),
                Text::raw("\n\n"),
                Text::raw("Tags: \""),
                Text::raw(tags.join("\", \"")),
                Text::raw("\""),
            ];

            Paragraph::new(text.iter())
                .block(block.clone())
                .alignment(Alignment::Left)
                .wrap(true)
                .render(&mut f, chunks[1]);
        })?;

        match events.next()? {
            Event::Input(input) => match input {
                Key::Char('q') => break,
                Key::Esc => break,
                Key::Down => app.selected = (app.selected + 1) % app.surveys.len(),
                Key::Up => {
                    app.selected = if app.selected > 0 {
                        app.selected - 1
                    } else {
                        app.surveys.len() - 1
                    }
                }
                _ => {}
            },
            Event::Tick => {}
        }
    }

    Ok(())
}
