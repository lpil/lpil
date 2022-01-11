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
use tui::widgets::{Block, Borders, Paragraph, SelectableList, Tabs, Text, Widget};
use tui::Terminal;

use crate::event::{Event, Events};

#[derive(Debug, Clone)]
enum Mood {
    Happy,
    Meh,
    Sad,
}

impl Mood {
    pub fn from_usize(x: usize) -> Option<Mood> {
        match x {
            0 => Some(Mood::Happy),
            1 => Some(Mood::Meh),
            2 => Some(Mood::Sad),
            _ => None,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Mood::Happy => "happy",
            Mood::Meh => "meh",
            Mood::Sad => "sad",
        }
        .to_string()
    }
}

struct App {
    surveys: Vec<Survey>,
    survey_index: usize,
    mood_index: usize,
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
        // .get("https://b7f836b1-6cad-4f88-94d2-0ef92541f5f3.mock.pstmn.io/surveys")
        .get("http://api.happylabs.io/surveys")
        .send()?
        .json()?;

    if let Some(mood) = run_tui(surveys)? {
        send_feedback(mood)?;
    }
    Ok(())
}

fn send_feedback(tup: (Mood, Survey)) -> Result<(), failure::Error> {
    let (mood, survey) = tup;
    #[derive(Debug, Serialize, Deserialize)]
    struct Body {
        mood: String,
    }
    let url = format!("http://api.happylabs.io/surveys/{}/feedback", survey.id);

    dbg!(reqwest::Client::new()
        .post(&url)
        .json(&Body {
            mood: mood.clone().to_string()
        })
        .send()?);
    println!("Sent {} feedback to HappyLabs", mood.to_string());
    Ok(())
}

fn run_tui(surveys: Vec<Survey>) -> Result<Option<(Mood, Survey)>, failure::Error> {
    let stdout = io::stdout().into_raw_mode()?;
    let stdout = MouseTerminal::from(stdout);
    let stdout = AlternateScreen::from(stdout);
    let backend = TermionBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    terminal.hide_cursor()?;

    let events = Events::new();

    let mut app = App {
        mood_index: 0,
        survey_index: 0,
        surveys,
    };

    loop {
        terminal.draw(|mut f| {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints(
                    [
                        Constraint::Length(10),
                        Constraint::Length(3),
                        Constraint::Percentage(100),
                    ]
                    .as_ref(),
                )
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
                .select(Some(app.survey_index))
                .style(style)
                .highlight_style(style.fg(Color::Cyan).modifier(Modifier::BOLD))
                .highlight_symbol("😁")
                .render(&mut f, chunks[0]);

            Tabs::default()
                .block(block.clone())
                .titles(&["💚 Happy", "😒 Meh", "💔 Sad"])
                .select(app.mood_index)
                .highlight_style(Style::default().fg(Color::Cyan).modifier(Modifier::BOLD))
                .render(&mut f, chunks[1]);

            let Survey {
                description,
                title,
                tags,
                ..
            } = app.surveys[app.survey_index].clone();

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
                .render(&mut f, chunks[2]);
        })?;

        match events.next()? {
            Event::Input(input) => match input {
                Key::Char('q') | Key::Esc => break,
                Key::Down => app.survey_index = inc(app.survey_index, app.surveys.len()),
                Key::Up => app.survey_index = dec(app.survey_index, app.surveys.len()),
                Key::Right => app.mood_index = inc(app.mood_index, 3),
                Key::Left => app.mood_index = dec(app.mood_index, 3),
                Key::Char('\n') => {
                    return Ok(Mood::from_usize(app.mood_index)
                        .map(|m| (m, app.surveys[app.survey_index].clone())))
                }
                _ => {}
            },
            Event::Tick => {}
        }
    }

    Ok(None)
}

fn inc(x: usize, max: usize) -> usize {
    (x + 1) % max
}

fn dec(x: usize, max: usize) -> usize {
    if x > 0 {
        x - 1
    } else {
        max - 1
    }
}
